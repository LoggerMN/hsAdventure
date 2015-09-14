-- | Main entry point to the application.
module Main where

--import           System.IO       (BufferMode (..), hSetBuffering, stdin)
import qualified Model
import qualified View

main :: IO ()
main = View.gameRecur (View.ViewState "Welcome to Adventure!" (Model.istate View.vRooms) "Type 'help' for commands.")
--main = do
--    putStrLn $ show $ Model.tState
--    putStrLn $ show $ inside 31




