-- | Main entry point to the application.
module Main where

--import           System.IO       (BufferMode (..), hSetBuffering, stdin)
import qualified Model
import qualified View

data TState r = TState { status   :: String
                        ,resource :: r
                       } deriving (Show)

data R t = R { name  :: String
              ,value :: t
              } deriving (Show)

inside r = TState { status = "inside"
                   ,resource = r
                  }

main :: IO ()
main = View.gameRecur (View.ViewState "Welcome to Adventure!" (Model.istate View.vRooms) "Type 'help' for commands.")
--main = do
--    putStrLn $ show $ inside "string"
--    putStrLn $ show $ inside 31




