module Main where

import Ast
import Network.HTTP
import Text.JSON
import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )
import System.Environment ( getArgs )

url path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0175jv6XdpWdKm9pYxVcBgmSMCIlP4aVxQxZ3PqOvpsH1H"

getdata path body = do a <- simpleHTTP (postRequestWithBody (url path) "text/text" body)
                       getResponseBody a

data Train = Train {
  problemid :: String,
  problemsize :: Int,
  challenge :: String,
  operators :: OperatorSet
  }
             deriving ( Show )

getTrain :: Int -> IO Train
getTrain n =
  do let requestbody = "{\n\"size\":" ++ show n ++ "\n}\n"
     b <- getdata "train" requestbody
     putStrLn $ "request: " ++ requestbody
     putStrLn $ "response: " ++ b
     case decode b of
            Error e -> fail $ "We got Error back: " ++ e ++ " decoding " ++ show b
            Ok a -> do let toTrain :: JSValue -> Train
                           toTrain (JSObject o) = Train { problemid = myid,
                                                          problemsize = mysize,
                                                          challenge = ch,
                                                          operators = toOperatorSet ops }
                             where Ok myid = readJSON idjson
                                   Just idjson = lookup "id" (fromJSObject o)
                                   Ok ch = readJSON chjson
                                   Just chjson = lookup "challenge" (fromJSObject o)
                                   Ok mysize = readJSON szjson
                                   Just szjson = lookup "size" (fromJSObject o)
                                   ops = case readJSON opsjson of
                                     Ok x -> x
                                     Error e -> error $ e ++ " with " ++ show opsjson
                                   Just opsjson = lookup "operators" (fromJSObject o)
                       return $ toTrain a

saveTrain :: Train -> IO ()
saveTrain p =
  do let dir = "trainings/" ++ show (problemsize p) ++ "/" ++ problemid p
     createDirectoryIfMissing True dir
     let opsname = dir ++ "/operators"
     writeFile opsname $ show (toStrings $ operators p) ++ "\n"
     let chname = dir ++ "/challenge"
     writeFile chname $ challenge p ++ "\n"

main = do args <- getArgs
          let probsize = (read (head args))
          tr <- getTrain probsize
          putStrLn $ show tr
          putStrLn $ "./build-all.sh && time ./submit_parallel " ++ show (problemsize tr) ++
            " " ++ problemid tr ++ " test master"
          saveTrain tr
