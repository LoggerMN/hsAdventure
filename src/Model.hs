module Model where

data RoomId = Inside | Outside deriving (Eq, Show, Enum)

data Item = Cup | Stick deriving (Eq, Show, Enum)

data Direction = North | South | East | West deriving (Eq, Show, Enum)

data Exits = Exits { north :: Maybe RoomId
                    ,south :: Maybe RoomId
                    ,east  :: Maybe RoomId
                    ,west  :: Maybe RoomId
                    } deriving ( Show )

dirFunc :: Direction -> ( Exits -> Maybe RoomId )
dirFunc North = north
dirFunc South = south
dirFunc East  = east
dirFunc West  = west

data Room = Room { pid     :: RoomId
                  ,descr   :: String
                  ,shortd  :: String
                  ,visited :: Bool
                  ,exits   :: Exits
                  ,items   :: [Item]
                 } deriving (Show)

instance Eq Room where
        x == y = (pid x) == (pid y)

data Rooms = Rooms { inside  :: Room
                    ,outside :: Room
                   } deriving (Eq, Show)

data Status = Playing | GameOver deriving (Eq, Show, Enum)

roomFunc :: RoomId -> (Rooms -> Room)
roomFunc Inside = inside
roomFunc Outside = outside

updateRoom :: Rooms -> RoomId -> Room -> Rooms
updateRoom rs Inside n = rs { inside=n }
updateRoom rs Outside n = rs { outside=n }

data State = State { status      :: Status
                    ,location    :: RoomId
                    ,rooms       :: Rooms
                    ,inventory   :: [Item]
                    } deriving (Show)

room :: State -> Room
room s = roomFunc (location s) $ rooms s

updateCurRoom :: State -> Room -> State
updateCurRoom s r = s { rooms=updateRoom (rooms s) (location s) r }

setCurRoomVisited :: State -> State
setCurRoomVisited s = updateCurRoom s nr
    where nr = (room s) { visited=True }

clearCurRoomVisited :: State -> State
clearCurRoomVisited s = updateCurRoom s nr
    where nr = (room s) { visited=False }


curExits :: State -> Exits
curExits s = exits $ room s

curDirExit :: Direction -> State -> Maybe RoomId
curDirExit d s = dirFunc d $ curExits s

curItems :: State -> [Item]
curItems s = items $ room s

iInsideRm  = Room { pid = Inside
                   ,descr="in a small dark room. No windows and only a single door."
                   ,shortd="in the small dark room."
                   ,visited=False
                   ,exits = Exits { north = Just Outside
                                   ,south = Nothing
                                   ,east = Nothing
                                   ,west = Nothing }
                   ,items=[Cup] }

iOutsideRm = Room { pid = Outside
                   ,descr="in the cold barren tundra. A small nearby shack to the south the only sign of civilization."
                   ,shortd="outside."
                   ,visited=False
                   ,exits = Exits { north = Nothing
                                   ,south = Just Inside
                                   ,east = Nothing
                                   ,west = Nothing }
                   ,items=[Stick] }

istate = State { status=Playing
                ,location=Inside
                ,rooms=Rooms{ inside  = iInsideRm
                             ,outside = iOutsideRm
                            }
                ,inventory=[]
               }

