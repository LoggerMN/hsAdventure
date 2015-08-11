module Model where

data PlaceId = Inside | Outside deriving (Eq, Show, Enum)

data Item = Cup | Stick deriving (Eq, Show, Enum)

data Exits = Exits { north :: Maybe Place
                    ,south :: Maybe Place
                    ,east  :: Maybe Place
                    ,west  :: Maybe Place
                    } -- can this be made Foldable?

maybeShowPlace :: Bool -> String -> (Maybe Place) -> String
maybeShowPlace _ _ Nothing = ""
maybeShowPlace False s (Just p) = s ++ " = " ++ (show $ pid p)
maybeShowPlace True s (Just p) = ", " ++ ( maybeShowPlace False s (Just p) )

instance Show Exits where
    show Exits { north=n, south=s, east=e, west=w} = "Exits {" ++ nstr ++ sstr ++ estr ++ wstr ++ "}"
        where nstr = ( maybeShowPlace False "north" n )
              sstr = ( maybeShowPlace ( nstr /= "" ) "south" s )
              estr = ( maybeShowPlace ( nstr /= "" || sstr /= "" ) "east" e )
              wstr = ( maybeShowPlace ( nstr /= "" || sstr /= "" || estr /= "") "west" w )  -- seems like I should somehow be able to do a fold here.

data Place = Place { pid   :: PlaceId
                    ,exits :: Exits
                   }

instance Eq Place where
        x == y = (pid x) == (pid y)

instance Show Place where
        show Place { pid=i, exits=es } = "Place {pid = " ++ (show i) ++ ", exits = " ++ (show es) ++ "}"


inside   = Place { pid = Inside,    exits = Exits { north = Just outside, south = Nothing,     east = Nothing, west = Nothing } }
outside  = Place { pid = Outside,   exits = Exits { north = Nothing,      south = Just inside, east = Nothing, west = Nothing } }


data Pose = Standing | Sitting deriving (Eq, Enum, Show)

data Status = Playing | GameOver deriving (Eq, Show, Enum)

data State = State { status      :: Status
                    ,location    :: Place
                    ,pose        :: Pose
                    ,inv_items   :: [Item]
                    ,place_items :: [(PlaceId,[Item])]
                    ,visited     :: [Place]
                    }
                    | ExceptionState String State String deriving (Eq, Show)


istate = State { status=Playing, location=inside, pose=Standing, inv_items=[], place_items=[ (Inside,[Cup]), (Outside,[Stick]) ], visited=[] }

showIstate = show istate

