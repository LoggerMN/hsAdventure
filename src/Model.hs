module Model where

data RoomId = Inside | Outside deriving (Eq, Show, Enum)

data Item = Cup | Stick deriving (Eq, Show, Enum)

data Direction = North | South | East | West deriving (Eq, Show, Enum)

data Exits = Exits { north :: Maybe RoomId
                    ,south :: Maybe RoomId
                    ,east  :: Maybe RoomId
                    ,west  :: Maybe RoomId
                    } deriving ( Show )

--dirFunc :: Direction -> ( Exits -> Maybe RoomId )
dirFunc North = north
dirFunc South = south
dirFunc East  = east
dirFunc West  = west

data Room r = Room { pid      :: RoomId
                    ,resource :: r
                    ,visited  :: Bool
                    ,exits    :: Exits
                    ,items    :: [Item]
                   } deriving (Show)

instance Eq (Room r) where
        x == y = (pid x) == (pid y)

data Rooms r = Rooms { inside  :: Room r
                      ,outside :: Room r
                     } deriving (Eq, Show)

data Status = Playing | GameOver deriving (Eq, Show, Enum)

--roomFunc :: RoomId -> (Rooms r -> Room r)
roomFunc Inside = inside
roomFunc Outside = outside

--updateRoom :: Rooms r -> RoomId -> Room r -> Rooms r
updateRoom rs Inside n = rs { inside=n }
updateRoom rs Outside n = rs { outside=n }

data State r = State { status      :: Status
                      ,location    :: RoomId
                      ,rooms       :: Rooms r
                      ,inventory   :: [Item]
                     } deriving (Show)

--room :: State r -> Room r
room s = roomFunc (location s) $ rooms s

--updateCurRoom :: State r -> Room r -> State r
updateCurRoom s r = s { rooms=updateRoom (rooms s) (location s) r }

--setCurRoomVisited :: State r -> State r
setCurRoomVisited s = updateCurRoom s nr
    where nr = (room s) { visited=True }

--clearCurRoomVisited :: State r -> State r
clearCurRoomVisited s = updateCurRoom s nr
    where nr = (room s) { visited=False }


--curExits :: State r -> Exits
curExits s = exits $ room s

--curDirExit :: Direction -> State r -> Maybe RoomId
curDirExit d s = dirFunc d $ curExits s

--curItems :: State r -> [Item]
curItems s = items $ room s

iInsideRm r = Room { pid = Inside
                    ,resource = r
                    ,visited=False
                    ,exits = Exits { north = Just Outside
                                    ,south = Nothing
                                    ,east = Nothing
                                    ,west = Nothing }
                    ,items=[Cup] }

iOutsideRm r = Room { pid = Outside
                     ,resource = r
                     ,visited=False
                     ,exits = Exits { north = Nothing
                                     ,south = Just Inside
                                     ,east = Nothing
                                     ,west = Nothing }
                     ,items=[Stick] }

iRooms rs = Rooms{ inside  = iInsideRm $ rs !! 0
                  ,outside = iOutsideRm $ rs !! 1
                 }

istate rs = State { status=Playing
                   ,location=Inside
                   ,rooms=rs
                   ,inventory=[]
                  }

tRooms = iRooms [(),()]
tState = istate tRooms
