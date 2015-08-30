-- | Main entry point to the application.
module Main where

import           Control.DeepSeq
import           Control.Monad
import           System.IO       (BufferMode (..), hSetBuffering, stdin)
import qualified Model
import qualified Logic
import qualified View

--gameLazy :: IO()
--gameLazy = do
--        sin <- getContents
--        let cmds = lines sin
--        let fs = map toOp cmds
--        let inf_states = scanl (\s f -> f s) initialState fs
--        let states = takeWhile (\st -> location ( flushErrors st ) /= gameOver) inf_states
--        mapM_ (\s -> putStrLn $ show s) states
--        putStrLn $ show $ inf_states !! ( length states )

gameRecur :: Model.State -> IO()
gameRecur s = do
        putStrLn (show s)
        if (Model.status (Logic.flushErrors s)) == Model.GameOver then
                return ()
        else
                do
                        cmd <- getLine
                        gameRecur ( (View.toOp cmd) s )

main :: IO ()
--main = gameRecur initialState
--main = gameLazy
main = putStrLn $ View.showIstate

