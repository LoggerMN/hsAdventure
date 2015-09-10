-- | Main entry point to the application.
module Main where

import           Control.DeepSeq
import           Control.Monad
import           System.IO       (BufferMode (..), hSetBuffering, stdin)
import qualified Model
import qualified Logic
import qualified View

gameRecur :: Model.State -> IO()
gameRecur s = do
        if ( Model.visited $ Model.room s ) then putStrLn (View.showShort s) else putStrLn (View.showFull s)
--        putStrLn $ show s
        let s' = Model.setCurRoomVisited s
--        putStrLn $ show s'
        if (Model.status s') == Model.GameOver then
                return ()
        else
                do
                        cmd <- getLine
                        gameRecur ( (View.toOp cmd) s' )

main :: IO ()
main = gameRecur Model.istate

