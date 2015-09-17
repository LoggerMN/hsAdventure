module Logic where
import Model

--nextLocation :: Direction -> State r -> RoomId
nextLocation d s = case ( curDirExit d s ) of
        Nothing -> location s
        Just r  -> r

--go :: Direction -> State r -> State r
go d s = s { location = nextLocation d s }

--quit :: State r -> State r
quit s = s { status = GameOver }
