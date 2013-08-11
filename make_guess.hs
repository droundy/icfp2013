module Main where

import Data.Word ( Word64 )
import System.Exit ( exitSuccess )
import System.CPUTime ( getCPUTime )
import System.Random
import Control.Concurrent ( threadDelay )

import Ast
import Network.HTTP
import Text.JSON
import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )
import System.Environment ( getArgs )

url path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0175jv6XdpWdKm9pYxVcBgmSMCIlP4aVxQxZ3PqOvpsH1H"

getdata :: String -> String -> IO String
getdata path body = do if length body < 128
                         then putStrLn ("body is:\n" ++ body)
                         else putStrLn ("body length is: " ++ show (length body))
                       a <- simpleHTTP (postRequestWithBody (url path) "text/text" body)
                       response <- getResponseBody a
                       if take (length "Too many requests") response == "Too many requests"
                         then do putStrLn response
                                 putStrLn "Too many requests! (trying again)"
                                 threadDelay 5000000 -- five seconds
                                 getdata path body
                         else return response

data Problem = Problem {
  problemkind :: TrainOrProblem,
  problemid :: String,
  problemsize :: Int,
  operators :: OperatorSet,
  fast :: Bool,
  solved :: Bool
  }
             deriving ( Show )

submitEval :: Problem -> [Word64] -> IO [Word64]
submitEval p g =
  do d <- getdata "eval" ("{\"id\":" ++ show (problemid p) ++ ",\"arguments\":" ++ hexes g ++ "}")
     if length d < 128
       then putStrLn $ "Response was: " ++ d
       else putStrLn $ "Response was length " ++ show (length d)
     case decode d of
       Error e -> fail $ "Error: " ++ e
       Ok a -> do let pars :: JSValue -> [Word64]
                      pars (JSObject o) = map read strings
                        where Just outs = lookup "outputs" (fromJSObject o)
                              Ok strings = readJSON outs
                  return $ pars a

submitGuess :: Problem -> Ast -> IO (Maybe (Word64, Word64, Word64))
submitGuess prob p =
  do d <- getdata "guess" ("{\"id\":" ++ show (problemid prob) ++ ",\"program\":" ++ show (lispify p) ++ "}")
     putStrLn d
     case decode d of
       Error e -> fail $ "submitGuess error " ++ e
       Ok (JSObject a) ->
         case lookup "status" (fromJSObject a) of
           Nothing -> fail "shoudlnt' be possible"
           Just statstr ->
             case readJSON statstr of
               Ok "win" -> return Nothing
               Ok "error" -> fail "error says the status"
               Ok "mismatch" ->
                 case lookup "values" (fromJSObject a) of
                   Nothing -> fail "cannot happend"
                   Just valstr ->
                     case readJSON valstr of
                       Ok (a,b,c) -> return (Just (read a, read b, read c))

problemDir :: Problem -> String
problemDir p = kdir ++ show (problemsize p) ++ "/" ++ problemid p ++ "/"
  where kdir = case problemkind p of DoTrain -> "trainings/"
                                     DoProblem -> "problems/"

makeGuess :: Problem -> [Ast] -> IO ()
makeGuess _ [] = fail "I have no idea!"
makeGuess prob (b:bs) =
  do r <- submitGuess prob b
     case r of
       Nothing -> do putStrLn "We won, we won!!!"
                     writeFile (problemDir prob ++ "solved") "solved\n"
       Just (inp, out, _) ->
         do putStrLn $ "I could do better on " ++ niceHex inp
            makeGuess prob (filter (\p -> eval p inp == out) bs)

data TrainOrProblem = DoTrain | DoProblem
                    deriving ( Show, Read, Eq )

readInfo :: TrainOrProblem -> Int -> String -> IO Problem
readInfo which sz ident =
  do let prob = Problem {
           problemkind = which,
           problemid = ident,
           problemsize = sz,
           operators = empty,
           fast = False,
           solved = False }
     ops <- readFile (problemDir prob ++ "operators")
     alreadydone <- doesFileExist (problemDir prob ++ "solved")
     isfast <- doesFileExist (problemDir prob ++ "fast")
     return prob { operators = toOperatorSet $ read ops, solved = alreadydone, fast = isfast }

main = do nstr:i:args <- getArgs
          let todo = case args of
                [] -> ""
                _ | "time" `elem` args -> "time"
                  | "count-programs" `elem` args -> "count-programs"
                  | "fast" `elem` args -> "fast"
                  | "problem" `elem` args -> ""
              kind = if "problem" `elem` args
                     then DoProblem
                     else DoTrain
              n = read nstr
          tr <- readInfo kind n i
          if solved tr then putStrLn "It is already solved!" else return ()
          putStrLn $ show tr
          start <- timeMe "File IO" 0
          case todo of
            "time" ->
                 do let programs = enumerate_program (problemsize tr) (operators tr)
                        rndprograms = filter isrnd programs
                        isrnd p = map (eval p) guesses == rndout
                        rndout = take (length guesses) $ randoms (mkStdGen 1)
                    printNumber $ length rndprograms
                    start <- timeMe "Filtering random programs" start
                    exitSuccess
            "count-programs" ->
                 do let programs = enumerate_program (problemsize tr) (operators tr)
                    printNumber (length programs)
                    start <- timeMe "Counting programs" start
                    exitSuccess
            "" -> do if solved tr then fail "We already solved it."
                                  else return ()
                     a <- submitEval tr guesses
                     let programs = enumerate_program (problemsize tr) (operators tr)
                     makeGuess tr $ filter (\p -> map (eval p) guesses == a) programs
            "fast" -> do if solved tr then fail "We already solved it."
                                      else return ()
                         if not (fast tr)
                           then putStrLn "It is not fast."
                           else do a <- submitEval tr guesses
                                   let programs = enumerate_program (problemsize tr) (operators tr)
                                   makeGuess tr $ filter (\p -> map (eval p) guesses == a) programs


timeMe :: String -> Integer -> IO Integer
timeMe job start =
  do stop <- getCPUTime
     let totalseconds = fromIntegral (stop-start)/1.0e12 :: Double
         minutes = floor (totalseconds/60)
         seconds = floor (totalseconds - 60*fromIntegral minutes)
     if minutes == 0
       then putStrLn $ job ++ " took " ++ show seconds ++ " seconds"
       else putStrLn $ job ++ " took " ++ show minutes ++ " and " ++ show seconds ++ " seconds"
     getCPUTime

printNumber :: Int -> IO ()
printNumber nprograms =
  if nprograms > 1024*1024*1024
  then putStrLn $ "I count " ++
       show (round $ fromIntegral nprograms/1024.0/1024.0/1024.0) ++ " gigaprograms"
  else if nprograms > 1024*1024
       then putStrLn $ "I count " ++
            show (round $ fromIntegral nprograms/1024.0/1024.0) ++ " megaprograms"
       else if nprograms > 1024
            then putStrLn $ "I count " ++
                 show (round $ fromIntegral nprograms/1024.0) ++ " kiloprograms"
            else putStrLn $ "I count " ++
                 show (round $ fromIntegral nprograms) ++ " programs"
