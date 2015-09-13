module View where
import Model
import Logic

data ViewState = ViewState String State String deriving (Show)

showRoomExit :: String -> (Maybe RoomId) -> String
showRoomExit _ Nothing = ""
showRoomExit dname (Just e) = dname

showRoomExitsDir :: String -> Exits -> String
showRoomExitsDir d e
        | d == "north" = showRoomExit d (north e) ++ (showRoomExitsDir "south" e)
        | d == "south" = showRoomExit d (south e) ++ (showRoomExitsDir "east" e)
        | d == "east"  = showRoomExit d (east e) ++ (showRoomExitsDir "west" e)
        | d == "west"  = showRoomExit d (west e)

showRoomExits :: Room -> String
showRoomExits r = "Exits are " ++ (showRoomExitsDir "north" ( exits r ) ) ++ "\n"

showExits :: State -> String
showExits s = showRoomExits $ room s

showPreSuffix s
    | s == "" = ""
    | otherwise = s ++ "\n"

showFullState :: ViewState -> String
showFullState (ViewState pre s post) = (showPreSuffix pre) ++ "You are " ++ (descr r) ++ "\n" ++ (showPreSuffix post)
    where r = room s

showShortState :: ViewState -> String
showShortState (ViewState pre s post) = (showPreSuffix pre) ++ "You are " ++ (shortd r) ++ "\n" ++ (showPreSuffix post)
    where r = room s

showState :: ViewState -> String
showState v@(ViewState _ s _) = if ( visited $ room s ) then (showShortState v) else (showFullState v)

showItems :: State -> String
showItems s = "Looking around you see " ++ (foldl (\a i -> (a ++ "a " ++ (show i) ++ " ")) "" $ curItems s)

showHelp :: ViewState -> String
showHelp s = "Commands are: north, south, east, west, exits, look, quit, help"

doCmd :: String -> ViewState -> ViewState
doCmd "north" v@(ViewState _ s _) = ViewState "" (go North s) "" 
doCmd "south" v@(ViewState _ s _) = ViewState "" (go South s) ""
doCmd "east"  v@(ViewState _ s _) = ViewState "" (go East s) ""
doCmd "west"  v@(ViewState _ s _) = ViewState "" (go West s) ""
doCmd "exits" v@(ViewState _ s _) = ViewState "" s (showExits s)
doCmd "look"  v@(ViewState _ s _) = ViewState "" s (showItems s)
doCmd "quit"  v@(ViewState _ s _) = ViewState "" (quit s) "Quitters never prosper" 
doCmd "help"  v@(ViewState _ s _) = ViewState "" s (showHelp v)
doCmd c       v@(ViewState _ s _) = ViewState ("I don't know how to " ++ c ++ ".") s ""

gameRecur :: ViewState -> IO()
gameRecur v@(ViewState _ s _) = do
        putStrLn $ showState v
--        putStrLn $ show s
        let v'@(ViewState _ s' _) = ViewState "" (Model.setCurRoomVisited s) ""
--        putStrLn $ show s'
        if (Model.status s') == Model.GameOver then
                return ()
        else
                do
                        cmd <- getLine
                        gameRecur ( (doCmd cmd) v' )


