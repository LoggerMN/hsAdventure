module View where
import Model
import Logic

data ViewState = ViewState (Maybe String) State (Maybe String) deriving (Show)

showExit :: String -> (Maybe RoomId) -> String
showExit _ Nothing = ""
showExit dname (Just e) = dname

showExitsDir :: String -> Exits -> String
showExitsDir d e
        | d == "north" = showExit d (north e) ++ (showExitsDir "south" e)
        | d == "south" = showExit d (south e) ++ (showExitsDir "east" e)
        | d == "east"  = showExit d (east e) ++ (showExitsDir "west" e)
        | d == "west"  = showExit d (west e)

showExits :: Room -> String
showExits r = "Exits are " ++ (showExitsDir "north" ( exits r ) )

showFull :: State -> String
showFull (ExceptionState pre s post) = pre ++ "\n\n" ++ (showFull s) ++ "\n\n" ++ post
showFull s = "You are " ++ (descr r) ++ "\n" ++ (showExits r)
    where r = room s

showShort :: State -> String
showShort (ExceptionState pre s post) = pre ++ "\n\n" ++ (showShort s) ++ "\n\n" ++ post
showShort s = "You are " ++ (shortd r) ++ "\n"
    where r = room s

showItems :: State -> State
showItems (ExceptionState _ s _ ) = showItems s
showItems s = ExceptionState "" ( clearCurRoomVisited s ) $ "Looking around you see " ++ (foldl (\a i -> (a ++ "a " ++ (show i) ++ " ")) "" $ curItems s)
    
help :: State -> State
help (ExceptionState _ s _) = help s
help s                    = ExceptionState ("Commands are: " ++ cs) s ""
    where cs = foldl (\a (n,f) -> (a ++ n ++ " ")) "" grammer

cmdExits :: State -> State
cmdExits (ExceptionState _ s _) = cmdExits s
cmdExits s                    = ExceptionState ( showExits (room s) ) s ""

grammer = [  ("stand", setPose Standing)
            ,("sit",   setPose Sitting)
            ,("north", go North)
            ,("south", go South)
            ,("east",  go East)
            ,("west",  go West)
            ,("exits", cmdExits)
            ,("look",  showItems)
            ,("quit",  quit)
            ,("help",  help)
            ]


toOp :: String -> ( State -> State )
toOp c = case m of
    Nothing  -> noOp
    (Just f) -> f
    where m = lookup c grammer


