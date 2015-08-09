-- | Main entry point to the application.
module Main where

import System.IO (hSetBuffering,stdin,BufferMode(..))
import Control.Monad
import Control.DeepSeq

data Place = Place {	 name  :: String
						,north :: Maybe Place
						,south :: Maybe Place
						,east  :: Maybe Place
						,west  :: Maybe Place
						}
	
showExit :: String -> (Maybe Place) -> String
showExit _ Nothing = ""
showExit dname (Just p) = "to the " ++ dname

showExitsDir :: String -> Place -> String
showExitsDir d p
	| d == "north" = showExit d (north p) ++ (showExitsDir "south" p)
	| d == "south" = showExit d (south p) ++ (showExitsDir "east" p)
	| d == "east"  = showExit d (east p) ++ (showExitsDir "west" p)
	| d == "west"  = showExit d (west p)

showExits :: Place -> String
showExits p = "Exits are\n" ++ (showExitsDir "north" p)

inside  = Place { name = "inside",  north = Just outside, south = Nothing,     east = Nothing, west = Nothing }
outside = Place { name = "outside", north = Nothing,      south = Just inside, east = Nothing, west = Nothing }
gameOver = Place { name = "Game Over", north = Nothing, south = Nothing, east = Nothing, west = Nothing }

instance Eq Place where
	x == y = (name x) == (name y)
	
instance Show Place where
	show p = name p 

data Position = Standing | Sitting | Laying deriving (Eq, Read, Enum)

instance Show Position where
	show Standing  = "standing"
	show Sitting   = "sitting"
	show Laying    = "laying"
	
data State = State {  location :: Place
					, position :: Position
					} 
			| ExceptionState String State deriving (Eq)

instance Show State where
	show ( ExceptionState msg s ) = msg ++ "\n" ++ ( show s )
	show s                    
		| ( location s ) == gameOver  = show( location s )
		| otherwise                   = "You are " ++ show(position s) ++ " " ++ show( location s ) ++ "\n-> "
	
flushErrors :: State -> State
flushErrors (ExceptionState _ s) = flushErrors s
flushErrors s = s

nextLocation :: Maybe Place -> State -> State
nextLocation m (ExceptionState _ s) = nextLocation m s
nextLocation Nothing s          = ExceptionState "You can't go that way." s
nextLocation (Just p) s         = s { location = p }

go :: ( Place -> Maybe Place ) -> State -> State
go f ( ExceptionState msg s ) = go f s
go f s                    = nextLocation (f (location s) ) s

pose :: Position -> State -> State
pose p ( ExceptionState msg s ) = pose p s
pose p s
	| position s == p = ExceptionState ( "You are already " ++ show(p) ++ "." ) s
	| otherwise       = s { position = p }

initialState = State { location = inside, position = Sitting }

quit :: State -> State
quit (ExceptionState _ s) = quit s
quit s = ExceptionState "Quitters never prosper" ( s { location = gameOver } )

grammer = [  ("stand",   pose Standing)
		   , ("sit",   pose Sitting)
		   , ("lay",   pose Laying)
		   , ("north", go north)
	       , ("south", go south)
	       , ("east",  go east)
	       , ("west",  go west)
	       , ("exits", exits)
		   , ("quit", quit)
		   ]

exits :: State -> State
exits (ExceptionState _ s) = exits s
exits s = ExceptionState ( showExits (location s) ) s

noOp :: State -> State
noOp (ExceptionState _ s) = noOp s
noOp s = ExceptionState "Do What?" s

toOp :: String -> ( State -> State )
toOp c = case m of
			Nothing  -> noOp
			(Just f) -> f
		where m = lookup c grammer

gameLazy :: IO()
gameLazy = do
	sin <- getContents
	let cmds = lines sin
	let fs = map toOp cmds
	let inf_states = scanl (\s f -> f s) initialState fs
	let states = takeWhile (\st -> location ( flushErrors st ) /= gameOver) inf_states
	mapM_ (\s -> putStrLn $ show s) states
	putStrLn $ show $ inf_states !! ( length states )
	
gameRecur :: State -> IO()
gameRecur s = do
	putStrLn (show s)
	if (location (flushErrors s)) == gameOver then 
		return ()
	else
		do
			cmd <- getLine
			gameRecur ( (toOp cmd) s )

main :: IO ()
--main = gameRecur initialState
main = gameLazy
