module Ast where

import Numeric ( showHex )
import Data.Tuple ( swap )
import Data.Word ( Word8, Word16, Word64 )
import Data.Bits ( Bits(..) )
import qualified Data.Map as Map
import System.Random

import Data.Array.Unboxed ( UArray, listArray, amap )

import Debug.Trace

data Ast = Zero | One | X | Y | Z
         | If0 Ast Ast Ast | Fold Ast Ast Ast
         | Not Ast | Shl1 Ast | Shr1 Ast | Shr4 Ast | Shr16 Ast
         | And Ast Ast | Or Ast Ast | Xor Ast Ast | Plus Ast Ast
         deriving ( Read, Show, Eq, Ord )

data OperatorSet = OS !Word16
                 deriving ( Eq )


-- eval
eval :: Ast -> Word64 -> Word64
eval f x = eval3 f x 0 0

eval3 :: Ast -> Word64 -> Word64 -> Word64 -> Word64
eval3 Zero _ _ _ = 0
eval3 One _ _ _ = 1
eval3 X x _ _ = x
eval3 Y _ y _ = y
eval3 Z _ _ z = z
eval3 (If0 e a b) x y z
  | eval3 e x y z == 0 = eval3 a x y z
  | otherwise = eval3 b x y z
eval3 (Fold e1 e2 e3) x _ _ = eval3 e3 x a8 b8
  where a8 = unsafeShiftR a 56 .&. 0xff
        b8 = eval3 e3 x a7 b7
        a7 = unsafeShiftR a 48 .&. 0xff
        b7 = eval3 e3 x a6 b6
        a6 = unsafeShiftR a 40 .&. 0xff
        b6 = eval3 e3 x a5 b5
        a5 = unsafeShiftR a 32 .&. 0xff
        b5 = eval3 e3 x a4 b4
        a4 = unsafeShiftR a 24 .&. 0xff
        b4 = eval3 e3 x a3 b3
        a3 = unsafeShiftR a 16 .&. 0xff
        b3 = eval3 e3 x a2 b2
        a2 = unsafeShiftR a 8 .&. 0xff
        b2 = eval3 e3 x a1 b1
        a1 = a .&. 0xff
        b1 = eval3 e2 x 0 0
        a = eval3 e1 x 0 0
eval3 (Not e) x y z = complement (eval3 e x y z)
eval3 (Shl1 e) x y z = unsafeShiftL (eval3 e x y z) 1
eval3 (Shr1 e) x y z = unsafeShiftR (eval3 e x y z) 1
eval3 (Shr4 e) x y z = unsafeShiftR (eval3 e x y z) 4
eval3 (Shr16 e) x y z = unsafeShiftR (eval3 e x y z) 16
eval3 (And e1 e2) x y z = (eval3 e1 x y z) .&. (eval3 e2 x y z)
eval3 (Or e1 e2) x y z = (eval3 e1 x y z) .|. (eval3 e2 x y z)
eval3 (Xor e1 e2) x y z = (eval3 e1 x y z) `xor` (eval3 e2 x y z)
eval3 (Plus e1 e2) x y z = (eval3 e1 x y z) + (eval3 e2 x y z)





-- size:

sizeInternal :: Ast -> Int
sizeInternal Zero = 1
sizeInternal One = 1
sizeInternal X = 1
sizeInternal Y = 1
sizeInternal Z = 1
sizeInternal (If0 a b c) = 1 + sizeInternal a + sizeInternal b + sizeInternal c
sizeInternal (Fold a b c) = 2 + sizeInternal a + sizeInternal b + sizeInternal c
sizeInternal (Not e) = 1 + sizeInternal e
sizeInternal (Shl1 e) = 1 + sizeInternal e
sizeInternal (Shr1 e) = 1 + sizeInternal e
sizeInternal (Shr4 e) = 1 + sizeInternal e
sizeInternal (Shr16 e) = 1 + sizeInternal e
sizeInternal (And a b) = 1 + sizeInternal a + sizeInternal b
sizeInternal (Or a b) = 1 + sizeInternal a + sizeInternal b
sizeInternal (Xor a b) = 1 + sizeInternal a + sizeInternal b
sizeInternal (Plus a b) = 1 + sizeInternal a + sizeInternal b

size :: Ast -> Int
size e = sizeInternal e + 1

-- enumerate (requires a size and TWO OperatorSets (definitely and maybe))
enumerate_program :: Int -> OperatorSet -> [Ast]
enumerate_program n musthave = enumerate_expression (n-1) musthave musthave

enumerate_expression :: Int -> OperatorSet -> OperatorSet -> [Ast]
enumerate_expression n musthave mayhave
  | musthave `overlapsWith` op_tfold  =
   map (Fold X Zero) (enumerate_expression (n-4) fold_musthave fold_mayhave)
      where
        fold_musthave = musthave `difference` op_tfold
        fold_mayhave = mayhave `difference` op_tfold `union` op_yz
enumerate_expression 1 musthave mayhave
  | musthave /= empty = [] -- we can't have musthaves here!
  | mayhave `overlapsWith` op_yz = [Zero, One, X, Y, Z]
  | otherwise = [Zero, One, X]
enumerate_expression 2 musthave mayhave
  | musthave `overlapsWith` ops_binary_trinary = []
  | musthave `overlapsWith` ops_unary =
    case distinctOperators $ intersection musthave ops_unary of
        [myop] -> apply_single_unary myop (enumerate_expression 1 (musthave `difference` myop) mayhave)
        _ -> []
  | otherwise = concatMap thingsfor unaries
      where unaries = filter (overlapsWith ops_unary) $ distinctOperators mayhave
            thingsfor myop = apply_single_unary myop (enumerate_expression 1 (musthave `difference` myop) mayhave)
enumerate_expression 3 musthave mayhave
  | musthave `overlapsWith` ops_trinary = []
  | musthave `overlapsWith` ops_binary && musthave `overlapsWith` ops_unary = []
  | musthave `overlapsWith` ops_binary =
    case distinctOperators $ intersection musthave ops_binary of
        [myop] -> apply_single_binary myop (enumerate_expression 1 (musthave `difference` myop) mayhave)
        _ -> []
  | length (distinctOperators (intersection musthave ops_unary)) > 1 =
      case distinctOperators $ intersection musthave ops_unary of
        [op1,op2] ->  apply_single_unary op1 (apply_single_unary op2 args) ++
                     apply_single_unary op2 (apply_single_unary op1 args)
          where args = enumerate_expression 1 (musthave `difference` (union op1 op2)) mayhave
        _ -> []
  | otherwise = (concatMap binary_asts binaries) ++ (concatMap unary_asts unaries)
      where binaries = filter (overlapsWith ops_binary) $ distinctOperators mayhave
            binary_asts myop = apply_single_binary myop (enumerate_expression 1 musthave mayhave)
            unaries = filter (overlapsWith ops_unary) $ distinctOperators mayhave
            unary_asts myop = apply_single_unary myop (enumerate_expression 2 (musthave `difference` myop) mayhave)
enumerate_expression n musthave mayhave
  | minimum_size musthave > n = []
  | otherwise = unary_tree ++ binary_tree ++ if_tree ++ fold_tree
  where
    fold_tree = [ Fold e1 e2 e3 |
                 i <- [1..(n-2-2)],
                 e1 <- enumerate_expression i empty fold_may,
                 let e1_ops = find_ast_ops e1,
                 -- Now let us check that we can possibly satisfy the remaining constraints:
                 True <- [minimum_size (fold_must `difference` e1_ops) <= n-2-i],
                 j <- [1..(n-2-i-1)],
                 e2 <- enumerate_expression j empty fold_may,
                 let e2_ops = find_ast_ops e2,
                 e3 <- enumerate_expression (n-2-i-j)
                       (fold_must `difference` (e1_ops `union` e2_ops))
                       (fold_may `union` op_yz) ]
      where fold_must = musthave `difference` op_fold
            fold_may = mayhave `difference` op_fold

    if_tree = [ If0 e1 e2 e3 |
               i <- [1..(n-1-2)],
               e1 <- enumerate_expression i empty mayhave,
               let e1_ops = find_ast_ops e1,
               j <- [1..(n-1-i-1)],
               e2 <- enumerate_expression j empty mayhave,
               let e2_ops = find_ast_ops e2,
               e3 <- enumerate_expression (n-1-i-j)
                     (musthave `difference` (e1_ops `union` (e2_ops `union` op_if))) mayhave ]
    binary_tree = [ apply_binary myop e1 e2 |
                    i <- [1..((n-1)`div`2)],
                    myop <- filter (overlapsWith ops_binary) $ distinctOperators mayhave,
                    e1 <- enumerate_expression i empty mayhave,
                    let e1_ops = find_ast_ops e1,
                    e2 <- enumerate_expression (n-1-i)
                          (musthave `difference` (union e1_ops myop)) mayhave]
    unary_tree = [apply_unary myop e1 |
                  myop <- filter (overlapsWith ops_unary) $ distinctOperators mayhave,
                  e1 <- enumerate_expression (n-1)
                        (musthave `difference` myop) mayhave ]

minimum_size :: OperatorSet -> Int
minimum_size o = 1 + (fromEnum $ o `overlapsWith` op_not)
                 + (fromEnum $ o `overlapsWith` op_shl1)
                 + (fromEnum $ o `overlapsWith` op_shr1)
                 + (fromEnum $ o `overlapsWith` op_shr4)
                 + (fromEnum $ o `overlapsWith` op_shr16)
                 + 2*((fromEnum $ o `overlapsWith` op_plus)
                      + (fromEnum $ o `overlapsWith` op_or)
                      + (fromEnum $ o `overlapsWith` op_xor)
                      + (fromEnum $ o `overlapsWith` op_and))
                 + 3*(fromEnum $ o `overlapsWith` op_if)
                 + 4*(fromEnum $ o `overlapsWith` op_fold)

apply_binary :: OperatorSet -> Ast -> Ast -> Ast
apply_binary o a b
  | o == op_plus = Plus a b
  | o == op_or = Or a b
  | o == op_xor = Xor a b
  | o == op_and = And a b

apply_unary :: OperatorSet -> Ast -> Ast
apply_unary o e
  | o == op_not = Not e
  | o == op_shl1 = Shl1 e
  | o == op_shr1 = Shr1 e
  | o == op_shr4 = Shr4 e
  | o == op_shr16 = Shr16 e

apply_single_binary :: OperatorSet -> [Ast] -> [Ast]
apply_single_binary o xs
  | o == op_plus = [Plus a b | a <- xs, b <- xs]
  | o == op_or = [Or a b | a <- xs, b <- xs]
  | o == op_xor = [Xor a b | a <- xs, b <- xs]
  | o == op_and = [And a b | a <- xs, b <- xs]
  | otherwise = []

apply_single_unary :: OperatorSet -> [Ast] -> [Ast]
apply_single_unary o xs
  | o == op_not = map Not xs
  | o == op_shl1 = map Shl1 xs
  | o == op_shr1 = map Shr1 xs
  | o == op_shr4 = map Shr4 xs
  | o == op_shr16 = map Shr16 xs
  | otherwise = []

-- OperatorSet from Ast

find_ast_ops :: Ast -> OperatorSet
find_ast_ops (Not e) = op_not `union` (find_ast_ops e)
find_ast_ops (Shl1 e) = op_shl1 `union` (find_ast_ops e)
find_ast_ops (Shr1 e) = op_shr1 `union` (find_ast_ops e)
find_ast_ops (Shr4 e) = op_shr4 `union` (find_ast_ops e)
find_ast_ops (Shr16 e) = op_shr16 `union` (find_ast_ops e)
find_ast_ops (Plus a b) = op_plus `union` (find_ast_ops a) `union` (find_ast_ops b)
find_ast_ops (Or a b) = op_or `union` (find_ast_ops a) `union` (find_ast_ops b)
find_ast_ops (Xor a b) = op_xor `union` (find_ast_ops a) `union` (find_ast_ops b)
find_ast_ops (And a b) = op_and `union` (find_ast_ops a) `union` (find_ast_ops b)
find_ast_ops (Fold a b c) = op_fold `union` (find_ast_ops a) `union` (find_ast_ops b) `union` (find_ast_ops c)
find_ast_ops (If0 a b c) = op_if `union` (find_ast_ops a) `union` (find_ast_ops b) `union` (find_ast_ops c)
find_ast_ops Y = op_yz
find_ast_ops Z = op_yz
find_ast_ops _ = empty

-- lisp output
toLisp :: Ast -> String
toLisp Zero = "0"
toLisp One = "1"
toLisp X = "x"
toLisp Y = "y"
toLisp Z = "z"
toLisp (If0 e a b)  = "(if0 "++toLisp e++" "++toLisp a++" "++toLisp b++")"
toLisp (Fold e a b) = "(fold "++toLisp e++" "++toLisp a++" "++"(lambda (y z) "++toLisp b++"))"

toLisp (Not e) = "(not "++toLisp e++")"
toLisp (Shl1 e) = "(shl1 "++toLisp e++")"
toLisp (Shr1 e) = "(shr1 "++toLisp e++")"
toLisp (Shr4 e) = "(shr4 "++toLisp e++")"
toLisp (Shr16 e) = "(shr16 "++toLisp e++")"

toLisp (And a b) = "(and "++toLisp a++" "++toLisp b++")"
toLisp (Or a b) = "(or "++toLisp a++" "++toLisp b++")"
toLisp (Xor a b) = "(xor "++toLisp a++" "++toLisp b++")"
toLisp (Plus a b) = "(plus "++toLisp a++" "++toLisp b++")"

lispify :: Ast -> String
lispify e = "(lambda (x) "++toLisp e++")"

-- OperatorSet code:

ops_trinary = OS (1 + 2)
ops_binary = OS (128 + 256 + 512 + 1024)
ops_unary = OS (4 + 8 + 16 + 32 + 64)

ops_binary_trinary = OS (1 + 2 + 128 + 256 + 512 + 1024)

ops_all = OS (1 + 2 + 4 + 8 + 16 + 32 + 64 + 128 + 256 + 512 + 1024)

op_if = OS 1
op_fold = OS 2
op_not = OS 4
op_shl1 = OS 8
op_shr1 = OS 16
op_shr4 = OS 32
op_shr16 = OS 64
op_and = OS 128
op_or = OS 256
op_xor = OS 512
op_plus = OS 1024
op_yz = OS 2048
op_tfold = OS 4096

allops = [(op_if, "if0"),
          (op_fold, "fold"),
          (op_not, "not"),
          (op_shl1, "shl1"),
          (op_shr1, "shr1"),
          (op_shr4, "shr4"),
          (op_shr16, "shr16"),
          (op_and, "and"),
          (op_or, "or"),
          (op_xor, "xor"),
          (op_plus, "plus"),
          (op_tfold, "tfold"),
          (op_yz, "yz")]

empty = OS 0

-- union

union :: OperatorSet -> OperatorSet -> OperatorSet
union (OS a) (OS b) = OS (a .|. b)

unions :: [OperatorSet] -> OperatorSet
unions xs = OS $ foldl (.|.) 0 (map tow xs)
  where tow (OS x) = x

difference :: OperatorSet -> OperatorSet -> OperatorSet
difference (OS a) (OS b) = OS (a .&. complement b)

intersection :: OperatorSet -> OperatorSet -> OperatorSet
intersection (OS a) (OS b) = OS (a .&. b)

overlapsWith :: OperatorSet -> OperatorSet -> Bool
a `overlapsWith` b = intersection a b /= empty

distinctOperators :: OperatorSet -> [OperatorSet]
distinctOperators (OS x) = dops 0 x
  where dops :: Int -> Word16 -> [OperatorSet]
        dops _ 0 = []
        dops shiftby z | (z .&. 1) == 1 = OS (unsafeShiftL 1 shiftby) : dops (shiftby + 1) (unsafeShiftR z 1)
                       | otherwise = dops (shiftby + 1) (unsafeShiftR z 1)

instance Show OperatorSet where
    showsPrec _ (OS x) = showString $ unwords $ map snd $ filter ok allops
      where ok (OS o, _) = x .&. o == o

toOperatorSet :: [String] -> OperatorSet
toOperatorSet [] = OS 0
toOperatorSet (x:xs) = xop `union` toOperatorSet xs
  where xop = case lookup x (map swap allops) of Nothing -> error ("bad value: " ++ x)
                                                 Just o -> o

toStrings :: OperatorSet -> [String]
toStrings o = map show $ distinctOperators o

randoms64 :: [Word64]
randoms64 = randoms (mkStdGen 0)

guesses :: [Word64]
guesses = take 256 $ [0, 3, 5, 6, 0xffffffffffffffff] ++
          map (\x -> unsafeShiftL 1 x) [0..63] ++
          map (\x -> complement (unsafeShiftL 1 x)) [0..63] ++ randoms64

betterguesses :: Int -> UArray Int Word64
betterguesses i | i > 256 = error "too big in betterguesses"
                | otherwise = listArray (0,i-1) guesses

eval_array :: Ast -> UArray Int Word64 -> UArray Int Word64
eval_array f = amap (eval f)

solver :: Int -> Int -> OperatorSet -> ([Word64], Map.Map [Word64] [Ast])
solver nguesses sz ops = (g, mp)
  where mp = Map.fromListWith (++) assoc_list
        assoc_list = map (\a -> (map (eval a) g , [a])) args
        args = enumerate_program sz ops
        g = take nguesses guesses

solver_array :: Int -> Int -> OperatorSet -> (UArray Int Word64, Map.Map (UArray Int Word64) [Ast])
solver_array nguesses sz ops = (g, mp)
  where mp = Map.fromListWith (++) assoc_list
        assoc_list = map (\a -> (amap (eval a) g , [a])) args
        args = enumerate_program sz ops
        g = betterguesses nguesses

niceHex :: Word64 -> String
niceHex x = "0x" ++ replicate (16 - length nonzero) '0' ++ nonzero
  where nonzero = showHex x ""

hexes :: [Word64] -> String
hexes = show . map niceHex
-- hexes xs = "[" ++ hx xs
--   where hx [] = "]"
--         hx [d] = niceHex d ++ "]"
--         hx (d:d2:ds) = niceHex d ++"," ++ hx (d2:ds)
