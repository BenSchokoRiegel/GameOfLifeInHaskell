
-- | Contains all types and data that are used in the game of life
module DataTypes where  

    import Graphics.Gloss.Data.Color
    import System.Random


    -- | A Cell Position / Coordinate
    --
    -- Upper left corner is (0, 0)
    type Position = (Int, Int)

    -- | A grid of cells
    type Grid = [Cell]

    -- | A single part of a 'Figure'
    --
    -- Example-Figure:
    --
    -- O X O  <- This is the first FigurePart
    --
    -- X X X  <- This is the second FigurePart
    --
    -- O X O  <- This is the third FigurePart
    type FigurePart = [Cell]

    -- | A figure that can be placed in the game of life.
    --
    -- Example-Figure:
    --
    -- O X O
    --
    -- X X X
    --
    -- O X O
    type Figure = [FigurePart]

    -- | A description of a 'Figure' using numbers.
    --
    -- This is used to create a 'Figure' (see 'Figures.toFigure').
    --
    --      * 0 means 'Dead'
    --      * any other number means 'Alive'
    type FigureDescription = [[Int]]

    -- | The data of a game state
    data GameData
        = GameData -- ^ Constructor for a game state
            { grid :: Grid -- ^ The grid with cells
            , running :: Bool -- ^ Whether the simulation is running
            , step :: Bool -- ^ Whether we should step one iteration
            , startGrid :: Grid -- ^ The starting grid of this game state
            , cColor :: Color -- ^ The cell color
            , showGrid :: Bool -- ^ Whether we should draw the wire grid
            -- | The type of 'Figure' that should be placed with the next click
            , nextFigure :: FigureType
            , stdGen :: StdGen } -- ^ The random number generator

    -- | A single Game of Life Cell. Either 'Dead' or 'Alive'.
    data Cell
        = Dead -- ^ Constructor for a 'Dead' Cell
        | Alive -- ^ Constructor for an 'Alive' Cell
        deriving (Eq, Show)

    -- | The different types of 'Figure's that can be placed in the simulation.
    data FigureType
        = None -- ^ Means that no figure is selected

        | Revive -- An Alive Cell
        | Kill -- A Dead Cell
        -- Still
        | Block -- ^ The static Block type
        | BeeHive -- ^ The static BeeHive type
        | Loaf -- ^ The static Loaf type
        | Boat -- ^ The static Boat type
        | Tub -- ^ The static Tub type

        -- Oscillating
        | Blinker -- ^ The oscillating Blinker type
        | Toad -- ^ The oscillating Toad type
        | Beacon -- ^ The oscillating Beacon type
        | Pulsar -- ^ The oscillating Pulsar type
        | Pentadecathlon -- ^ The oscillatingPentadecathlon type

        -- Spaceship
        | Glider -- ^ The Glider type
        | LightWeight -- ^ The light weight spaceship type
        | MiddleWeight -- ^ The middle weight spaceship type
        | HeavyWeight -- ^ The heavy weight spaceship type

        -- Other
        | Clock -- ^ The Clock type
        | DoubleU -- ^ The DoubleU type
        | RPentomino -- ^ The r-Pentomino type
        | BlockRow -- ^ The BlockRow type
        | LittleSwimmer -- ^ The little swimmer type
        | Swimmer -- ^ the swimmer type
        | WichStretcher -- ^ The wich stretcher type
        | GliderGun1 -- ^ The first glider gun type
        | GliderGun2 -- ^ The second glider gun type
        deriving (Eq, Show)