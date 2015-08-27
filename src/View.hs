module View where
import Model

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
showFull (ExceptionState pre s post) = (show pre) ++ (showFull s) ++ (show post)
showFull s = "You are " ++ (descr r) ++ "\n" ++ (showExits r)
    where r = room s

showShort :: State -> String
showShort (ExceptionState pre s post) = (show pre) ++ (showShort s) ++ (show post)
showShort s = "You are " ++ (shortd r) ++ "\n"
    where r = room s


