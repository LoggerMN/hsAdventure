{-# LANGUAGE FlexibleInstances #-}

module Model where

data RoomId = Inside | Outside deriving (Eq, Show, Enum)

data Item = Cup | Stick deriving (Eq, Show, Enum)

data Exits = Exits { north :: Maybe RoomId
                    ,south :: Maybe RoomId
                    ,east  :: Maybe RoomId
                    ,west  :: Maybe RoomId
                    } deriving ( Show )

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

data Pose = Standing | Sitting deriving (Eq, Enum, Show)

data Status = Playing | GameOver deriving (Eq, Show, Enum)

roomFunc :: RoomId -> (Rooms -> Room)
roomFunc Inside = inside
roomFunc Outside = outside

updateRoom :: Rooms -> RoomId -> Room -> Rooms
updateRoom rs Inside n = rs { inside=n }
updateRoom rs Outside n = rs { outside=n }

room :: State -> Room
room s = roomFunc (location s) $ rooms s

data State = State { status      :: Status
                    ,location    :: RoomId
                    ,pose        :: Pose
                    ,rooms       :: Rooms
                    ,inventory   :: [Item]
                    }
                    | ExceptionState String State String deriving (Show)


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
                   ,descr="in the cold barren tundra in all directions, with a small nearby shack the only sign of civilization."
                   ,shortd="outside."
                   ,visited=False
                   ,exits = Exits { north = Nothing
                                   ,south = Just Inside
                                   ,east = Nothing
                                   ,west = Nothing }
                   ,items=[Stick] }

istate = State { status=Playing
                ,location=Inside
                ,pose=Standing
                ,rooms=Rooms{ inside  = iInsideRm
                             ,outside = iOutsideRm
                            }
                ,inventory=[]
               }

