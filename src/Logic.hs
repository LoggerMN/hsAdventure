module Logic where
import Model

l = location istate
f = roomFunc l
rs = rooms istate
nrm = (f rs) { visited=True }
nstate = istate { rooms=updateRoom rs l nrm }

flushErrors :: State -> State
flushErrors (ExceptionState _ s _) = flushErrors s
flushErrors s = s

nextLocation :: Maybe RoomId -> State -> State
nextLocation m (ExceptionState _ s _ ) = nextLocation m s
nextLocation Nothing s                 = ExceptionState "You can't go that way." s ""
nextLocation (Just p) s                = s { location = p }

go :: Direction -> State -> State
go d ( ExceptionState _ s _ ) = go d s
go d s                        = nextLocation ( curExit d s ) s

setPose :: Pose -> State -> State
setPose p ( ExceptionState msg s _) = setPose p s
setPose p s
        | pose s == p = ExceptionState ( "You are already " ++ show(p) ++ "." ) s ""
        | otherwise       = s { pose = p }

quit :: State -> State
quit (ExceptionState _ s _) = quit s
quit s = ExceptionState "Quitters never prosper" ( s { status = GameOver } ) ""


