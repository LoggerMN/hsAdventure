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

showHelp :: String
showHelp = "Commands are: " ++ cs
    where cs = foldl (\a (n,f) -> (a ++ n ++ " ")) "" grammer


cmdHelp :: ViewState -> ViewState
cmdHelp (ViewState _ s _) = ViewState "" s showHelp

cmdExits :: ViewState -> ViewState
cmdExits (ViewState _ s _) = ViewState "" s (showExits s)

cmdLook :: ViewState -> ViewState
cmdLook (ViewState _ s _) = ViewState "" s (showItems s)

cmdQuit :: ViewState -> ViewState
cmdQuit (ViewState _ s _) = ViewState "" (quit s) "Quitters never prosper" 

cmdGo :: Direction -> ViewState -> ViewState
cmdGo d (ViewState _ s _) = ViewState "" (go d s) ""

grammer :: [ (String, (ViewState -> ViewState) ) ]
grammer = [  ("north", cmdGo North)
            ,("south", cmdGo South)
            ,("east",  cmdGo East)
            ,("west",  cmdGo West)
            ,("exits", cmdExits)
            ,("look",  cmdLook)
            ,("quit",  cmdQuit)
            ,("help",  cmdHelp)
            ]


doCmd :: String -> ViewState -> ViewState
doCmd c v@(ViewState _ s _) = case m of
    Nothing  -> ViewState ("I don't know how to " ++ c ++ ".") s ""
    (Just f) -> f v
    where m = lookup c grammer

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


