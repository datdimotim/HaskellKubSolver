module Benchmark where

import Data.UnixTime
import KubSolver
import KubUtils


type MsInterval = Int
type Randomizer a = IO a
type TaskSolver a s  = a -> s
type ResultExtractor s m = s -> m   

benchmark :: IO ()
benchmark = do 
              start <- getUnixTime
              r <- rndKub
              let s = head . kubSolver $ r
              print s
              finish <- getUnixTime
              let timeMs = (unixDiffTimeToMicros (finish `diffUnixTime` start)) `div` 1000
              putStrLn $ "time: " ++ show timeMs ++ "ms"
              putStrLn ""
              benchmark

{-

benchmark :: (Monoid m, Show m) => MsInterval -> Randomizer a -> TaskSolver a s -> ResultExtractor s m -> IO ()
benchmark msInterval randomer solver extractor = let 
                                                   iter = do
                                                            rnd <- randomer
                                                            let solution = solver rnd
                                                            let res = extractor solution
                                                            return res
                                                   helper startTime acc = do 
                                                                            r <- iter
                                                                            time <- getUnixTime
                                                                            let diffMillis = (unixDiffTimeToMicros $ time `diffUnixTime` startTime) `div` 1000
                                                                            if diffMillis > 
                                                 in 
                                                   undefined
                                                   
-} 
    