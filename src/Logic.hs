module Logic where
import Model

nextLocation :: Direction -> State -> RoomId
nextLocation d s = case ( curDirExit d s ) of
        Nothing -> location s
        Just r  -> r

go :: Direction -> State -> State
go d s = s { location = nextLocation d s }

quit :: State -> State
quit s = s { status = GameOver }
