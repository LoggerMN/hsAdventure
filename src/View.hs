module View where
import Model
import Logic

data ViewResource = ViewResource { descr  :: String
                                  ,shortd :: String
                                 } deriving (Show)

data ViewState = ViewState String (State ViewResource) String deriving (Show)

--showRoomExit :: String -> (Maybe RoomId) -> String
showRoomExit _ Nothing = ""
showRoomExit dname (Just e) = dname

--showRoomExitsDir :: String -> Exits -> String
showRoomExitsDir d e
        | d == "north" = showRoomExit d (north e) ++ (showRoomExitsDir "south" e)
        | d == "south" = showRoomExit d (south e) ++ (showRoomExitsDir "east" e)
        | d == "east"  = showRoomExit d (east e) ++ (showRoomExitsDir "west" e)
        | d == "west"  = showRoomExit d (west e)

--showRoomExits :: Room r -> String
showRoomExits r = "Exits are " ++ (showRoomExitsDir "north" ( exits r ) ) ++ "\n"

--showExits :: State r -> String
showExits s = showRoomExits $ room s

showPreSuffix s
    | s == "" = ""
    | otherwise = s ++ "\n"

--showFullState :: ViewState -> String
showFullState (ViewState pre s post) = (showPreSuffix pre) ++ "You are " ++ (descr $ resource r) ++ "\n" ++ (showPreSuffix post)
    where r = room s

--showShortState :: ViewState -> String
showShortState (ViewState pre s post) = (showPreSuffix pre) ++ "You are " ++ (shortd $ resource r) ++ "\n" ++ (showPreSuffix post)
    where r = room s

--showState :: ViewState -> String
showState v@(ViewState _ s _) = if ( visited $ room s ) then (showShortState v) else (showFullState v)

--showItems :: State r -> String
showItems s = "Looking around you see " ++ (foldl (\a i -> (a ++ "a " ++ (show i) ++ " ")) "" $ curItems s)

--showHelp :: ViewState -> String
showHelp s = "Commands are: north, south, east, west, exits, look, quit, help"

--doGo :: Direction -> State ViewResource -> ViewState
doGo d s = ViewState pre (go d s) ""
    where pre = if ( curDirExit d s ) == Nothing then "You can't go that way." else ""

--doCmd :: String -> ViewState -> ViewState
doCmd c v@(ViewState _ s _) = case c of "north" -> doGo North s
                                        "south" -> doGo South s
                                        "east"  -> doGo East s
                                        "west"  -> doGo West s
                                        "exits" -> ViewState "" s (showExits s)
                                        "look"  -> ViewState "" s (showItems s)
                                        "quit"  -> ViewState "" (quit s) "Quitters never prosper"
                                        "help"  -> ViewState "" s (showHelp v)
                                        c       -> ViewState ("I don't know how to " ++ c ++ ".") s ""

--gameRecur :: ViewState -> IO()
gameRecur v@(ViewState _ s _) = do
        putStrLn $ showState v
        let v'@(ViewState _ s' _) = ViewState "" (Model.setCurRoomVisited s) ""
        if (Model.status s') == Model.GameOver then
                return ()
        else
                do
                        cmd <- getLine
                        gameRecur ( (doCmd cmd) v' )


vResources = [ ViewResource { descr  = "in a small dark room. No windows and only a single door."
                             ,shortd = "in the small dark room."
                            }
              ,ViewResource { descr  ="in the cold barren tundra. A small nearby shack to the south the only sign of civilization."
                             ,shortd ="outside."
                            }
             ]

vRooms = iRooms vResources

