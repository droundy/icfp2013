module Main where

import System.Process
import Data.Word ( Word64 )
import System.Exit ( exitSuccess, ExitCode( ExitSuccess ) )
import System.CPUTime ( getCPUTime )
import System.Random
import Control.Concurrent ( threadDelay, myThreadId, killThread, forkIO )

import Ast
import Network.HTTP
import Text.JSON
import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )
import System.Environment ( getArgs )

url path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0175jv6XdpWdKm9pYxVcBgmSMCIlP4aVxQxZ3PqOvpsH1H"

data Speed = Fast | Slow

getdata :: Speed -> String -> String -> IO String
getdata speed path body =
                    do if length body < 128
                         then putStrLn ("body is:\n" ++ body)
                         else putStrLn ("body length is: " ++ show (length body))
                       a <- simpleHTTP (postRequestWithBody (url path) "text/text" body)
                       response <- getResponseBody a
                       if take (length "Too many requests") response == "Too many requests"
                         then do putStrLn response
                                 putStrLn "Too many requests! (trying again)"
                                 case speed of
                                   Slow -> threadDelay 20000000 -- twenty seconds
                                   Fast -> threadDelay  1000000 -- one seconds
                                 getdata speed path body
                         else return response

data Problem = Problem {
  problemkind :: TrainOrProblem,
  problemid :: String,
  problemsize :: Int,
  operators :: OperatorSet,
  solved :: Bool
  }
             deriving ( Show )

submitEval :: Problem -> [Word64] -> IO [Word64]
submitEval p g =
  do d <- getdata Slow "eval" ("{\"id\":" ++ show (problemid p) ++ ",\"arguments\":" ++ hexes g ++ "}")
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
  do d <- getdata Fast "guess" ("{\"id\":" ++ show (problemid prob) ++ ",\"program\":" ++ show (lispify p) ++ "}")
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

makeGuess :: String -> Problem -> [Ast] -> IO ()
makeGuess which_process _ [] = fail (which_process ++ " method failed!")
makeGuess which_process prob (b:bs) =
  do r <- submitGuess prob b
     case r of
       Nothing -> do putStrLn $ "We won, won!! " ++ which_process ++ " is the best!!!"
                     writeFile (problemDir prob ++ "solved") "solved\n"
       Just (inp, out, _) ->
         do putStrLn $ "I could do better on " ++ niceHex inp
            makeGuess which_process prob (filter (\p -> eval p inp == out) bs)

data TrainOrProblem = DoTrain | DoProblem
                    deriving ( Show, Read, Eq )

readInfo :: TrainOrProblem -> Int -> String -> IO Problem
readInfo which sz ident =
  do let prob = Problem {
           problemkind = which,
           problemid = ident,
           problemsize = sz,
           operators = empty,
           solved = False }
     ops <- readFile (problemDir prob ++ "operators")
     alreadydone <- doesFileExist (problemDir prob ++ "solved")
     return prob { operators = toOperatorSet $ read ops, solved = alreadydone }

srun = ""
methods = ["standard", "simple", "bonus", "11all", "7all"]

main =
  do nstr:i:test_or_prog:which_process:eval_results <- getArgs
     let kind = if test_or_prog == "program"
                then DoProblem
                else DoTrain
         n = read nstr :: Int
         a = map read eval_results
     tr <- readInfo kind n i
     if solved tr then putStrLn "It is already solved!" else return ()
     putStrLn $ show tr
     start <- timeMe "File IO" 0
     case which_process of
       "master" ->
         do results <- submitEval tr guesses
            let command_begin = srun ++ "./submit_parallel " ++ nstr ++ " " ++ i
                                ++ " " ++ test_or_prog ++ " "
                command_end = " " ++ (unwords $ map show results)
                commands = [command_begin ++ meth ++ command_end | meth <- methods]
            r <- mapM createProcess (map shell commands)
            mainid <- myThreadId
            let waitforsuccess procid =
                  do retval <- waitForProcess procid
                     case retval of
                       ExitSuccess -> do putStrLn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                         putStrLn "WE WON!!! WE WON!!! WE WON!!! WE WON!!! WE WON!!!"
                                         mapM_ (\(_,_,_,pid) -> terminateProcess pid) r
                                         killThread mainid
                       _ -> do putStrLn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                               putStrLn "A process died horribly..."
            mapM (\(_,_,_,pid) -> forkIO $ waitforsuccess pid) r
            threadDelay 400000000 -- hokey way of making sure it stays open for over mins (will be killed after 5 anyway)
            return ()
       "standard" ->
         do if solved tr then fail "We already solved it."
              else return ()
            do
               let programs = enumerate_program (problemsize tr) (operators tr)
               makeGuess which_process tr $ filter (\p -> map (eval p) guesses == a) programs
       "7all" ->
         do if solved tr then fail "We already solved it."
              else return ()
            do
               let programs = enumerate_all 7
               makeGuess which_process tr $ filter (\p -> map (eval p) guesses == a) programs
       "11all" ->
         do if solved tr then fail "We already solved it."
              else return ()
            do
               let programs = enumerate_all 11
               makeGuess which_process tr $ filter (\p -> map (eval p) guesses == a) programs
       "16all" ->
         do if solved tr then fail "We already solved it."
              else return ()
            do
               let programs = enumerate_all 16
               makeGuess which_process tr $ filter (\p -> map (eval p) guesses == a) programs
       "simple" ->
         do if solved tr then fail "We already solved it."
              else return ()
            do
               let programs = enumerate_all_simple 500
               makeGuess which_process tr $ filter (\p -> map (eval p) guesses == a) programs
       "bonus" ->
         do if solved tr then fail "We already solved it."
              else return ()
            do
               let programs = enumerate_bonus (problemsize tr) (operators tr)
               makeGuess which_process tr $ filter (\p -> map (eval p) guesses == a) programs

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
