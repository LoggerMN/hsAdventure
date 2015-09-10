module Logic where
import Model

flushExceptions :: State -> State
flushExceptions (ExceptionState _ s _) = flushExceptions s
flushExceptions s = s

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
quit s = ExceptionState "" ( s { status = GameOver } ) "Quitters never prosper"

noOp :: State -> State
noOp (ExceptionState _ s _) = noOp s
noOp s                    = ExceptionState "You can't do that." s ""

