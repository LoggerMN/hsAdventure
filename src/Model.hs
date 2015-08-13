{-# LANGUAGE FlexibleInstances #-}

module Model where

data RoomId = Inside | Outside deriving (Eq, Show, Enum)

data Item = Cup | Stick deriving (Eq, Show, Enum)

data Exits = Exits { north :: Maybe (Rooms -> Room)
                    ,south :: Maybe (Rooms -> Room)
                    ,east  :: Maybe (Rooms -> Room)
                    ,west  :: Maybe (Rooms -> Room)
                    } deriving ( Show )

data Room = Room { pid     :: RoomId
                  ,visited :: Bool
                  ,exits   :: Exits
                  ,items   :: [Item]
                 } deriving (Show)

instance Eq Room where
        x == y = (pid x) == (pid y)

data Rooms = Rooms { inside  :: Room
                    ,outside :: Room
                   } deriving (Eq)

instance Show Rooms where
    show rs = "\n         Rooms { inside=" ++ (show $ inside rs) ++ "\n                ,outside=" ++ (show $ outside rs) ++ "\n               }\n       "

data Pose = Standing | Sitting deriving (Eq, Enum, Show)

data Status = Playing | GameOver deriving (Eq, Show, Enum)

instance Show (Rooms -> Room) where
    show f = show $ pid $ f $ rooms istate

data State = State { status      :: Status
                    ,location    :: (Rooms -> Room)
                    ,pose        :: Pose
                    ,rooms       :: Rooms
                    ,inventory   :: [Item]
                    }
                    | ExceptionState String State String deriving (Show)

istate = State { status=Playing
                ,location=inside
                ,pose=Standing
                ,rooms=Rooms{ inside  = Room { pid = Inside,  visited=False, exits = Exits { north = Just outside, south = Nothing,     east = Nothing, west = Nothing }, items=[Cup] } 
                             ,outside = Room { pid = Outside, visited=False, exits = Exits { north = Nothing,      south = Just inside, east = Nothing, west = Nothing }, items=[Stick] }
                            }
                ,inventory=[]
               }

showIstate = show istate

