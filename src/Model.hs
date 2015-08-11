module Model where

data Item = Cup | Stick deriving (Eq, Show, Enum)

data Exits = Exits { north :: Maybe Place
                    ,south :: Maybe Place
                    ,east  :: Maybe Place
                    ,west  :: Maybe Place
                    }

maybeShowPlace :: String -> (Maybe Place) -> String
maybeShowPlace s Nothing = ""
maybeShowPlace s (Just p) = s ++ " " ++ (name p) ++ "\n"

instance Show Exits where
    show Exits { north=n, south=s, east=e, west=w} = ( maybeShowPlace "north" n ) ++ ( maybeShowPlace "south" s ) ++ ( maybeShowPlace "east" e ) ++ ( maybeShowPlace "west" w )

data Place = Place { name  :: String
                    ,exits :: Exits
                   }

instance Eq Place where
        x == y = (name x) == (name y)

instance Show Place where
        show Place { name=n, exits=es } = n ++ "\n" ++ (show es)


inside   = Place { name = "inside",    exits = Exits { north = Just outside, south = Nothing,     east = Nothing, west = Nothing } }
outside  = Place { name = "outside",   exits = Exits { north = Nothing,      south = Just inside, east = Nothing, west = Nothing } }


data Pose = Standing | Sitting deriving (Eq, Enum, Show)

data Status = Playing | GameOver deriving (Eq, Show, Enum)

data State = State { status      :: Status
                    ,location    :: Place
                    ,pose        :: Pose
                    ,inv_items   :: [Item]
                    ,place_items :: [(String,Item)]
                    ,visited     :: [Place]
                    }
                    | ExceptionState String State String deriving (Eq)


