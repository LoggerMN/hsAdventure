module Model where

data Item = Cup | Stick

data Exits = Exits { north :: Maybe Place
                    ,south :: Maybe Place
                    ,east  :: Maybe Place
                    ,west  :: Maybe Place
                    }

data Place = Place { name  :: String
                    ,exits :: Exits
                   }

inside   = Place { name = "inside",    exits = Exits { north = Just outside, south = Nothing,     east = Nothing, west = Nothing } }
outside  = Place { name = "outside",   exits = Exits { north = Nothing,      south = Just inside, east = Nothing, west = Nothing } }

instance Eq Place where
        x == y = (name x) == (name y)

instance Show Place where
        show p = name p

data Position = Standing | Sitting deriving (Eq, Read, Enum)

instance Show Position where
        show Standing  = "standing"
        show Sitting   = "sitting"

data Status = Starting | Playing | GameOver

data State = State { status      :: Status
                    ,location    :: Place
                    ,position    :: Position
                    ,inv_items   :: [Item]
                    ,place_items :: [(String,Item)]
                    }
                    | ExceptionState String State deriving (Eq)

instance Show State where
        show ( ExceptionState msg s ) = msg ++ "\n" ++ ( show s )
        show s
            | ( location s ) == gameOver  = show( location s )
            | otherwise                   = "You are " ++ show(position s) ++ " " ++ show( location s ) ++ "\n-> "

