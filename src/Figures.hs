
-- | Defines all the figures that can be placed in the simulation.
module Figures where
    import DataTypes

    -- | Returns the figure for a given 'FigureType'
    figureForType ::
        FigureType -- ^ The 'FigureType'
        -> Figure -- The resulting figure
    figureForType fType = case fType of
        Block -> block
        BeeHive -> beeHive
        Loaf -> loaf
        Boat -> boat
        Tub -> tub
        Blinker -> blinker
        Toad -> toad
        Beacon -> beacon
        Pulsar -> pulsar
        Pentadecathlon -> pentadecathlon
        Glider -> glider
        LightWeight -> lightWeight
        MiddleWeight -> middleWeight
        HeavyWeight -> heavyWeight
        Clock -> clock
        DoubleU -> doubleU
        RPentomino -> rPentomino
        BlockRow -> blockRow
        LittleSwimmer -> littleSwimmer
        Swimmer -> swimmer
        WichStretcher -> wichStretcher
        _ -> []

    -- | Returns the 'FigureType' that is succeeding the given type
    --
    -- This is the order:
    -- 
    --      * None
    --      * Block
    --      * BeeHive
    --      * Loaf
    --      * Boat
    --      * Tub
    --      * Blinker
    --      * Toad
    --      * Beacon
    --      * Pulsar
    --      * Pentadecathlon
    --      * Glider
    --      * LightWeight
    --      * MiddleWeight
    --      * HeavyWeight
    --      * Clock
    --      * DoubleU
    --      * RPentomino
    --      * BlockRow
    --      * LittleSwimmer
    --      * Swimmer
    --      * WichStretcher
    --      * GliderGun1
    --      * GliderGun2
    nextFig ::
        FigureType -- ^ The type to get the successor of
        -> FigureType -- ^ The successor
    nextFig fType = case fType of
        None -> Block
        Block -> BeeHive
        BeeHive -> Loaf
        Loaf -> Boat
        Boat -> Tub
        Tub -> Blinker
        Blinker -> Toad
        Toad -> Beacon
        Beacon -> Pulsar
        Pulsar -> Pentadecathlon
        Pentadecathlon -> Glider
        Glider -> LightWeight
        LightWeight -> MiddleWeight
        MiddleWeight -> HeavyWeight
        HeavyWeight -> Clock
        Clock -> DoubleU
        DoubleU -> RPentomino
        RPentomino -> BlockRow
        BlockRow -> LittleSwimmer
        LittleSwimmer -> Swimmer
        Swimmer -> WichStretcher
        WichStretcher -> GliderGun1
        GliderGun1 -> GliderGun2
        _ -> None

    backFig ::
        FigureType -- ^ The type to get the successor of
        -> FigureType -- ^ The successor
    backFig fType = case fType of
        None -> GliderGun2
        Block -> GliderGun2
        BeeHive -> Block
        Loaf ->  BeeHive
        Boat -> Loaf
        Tub -> Boat
        Blinker ->  Tub
        Toad-> Blinker
        Beacon ->  Toad
        Pulsar -> Beacon
        Pentadecathlon -> Pulsar
        Glider->  Pentadecathlon
        LightWeight-> Glider
        MiddleWeight -> LightWeight
        HeavyWeight-> MiddleWeight
        Clock -> HeavyWeight
        DoubleU -> Clock
        RPentomino -> DoubleU
        BlockRow -> RPentomino
        LittleSwimmer -> BlockRow
        Swimmer -> LittleSwimmer
        WichStretcher -> Swimmer
        GliderGun1 -> WichStretcher
        GliderGun2 -> GliderGun1

    -- | Turns a 'FigureDescription' into a 'Figure'
    toFigure :: FigureDescription -> Figure
    toFigure [] = []
    toFigure (x:xs) = map (\v -> if v == 0 then Dead else Alive) x : toFigure xs

    
    ---------------------------------------------
    -- Still
    ---------------------------------------------

    -- | The figure for a still block
    block :: Figure
    block = toFigure
        [ [ 1, 1 ]
        , [ 1, 1 ] ]

    -- | The figure for a still bee hive
    beeHive :: Figure
    beeHive = toFigure
        [ [ 0, 1, 1, 0 ]
        , [ 1, 0, 0, 1 ]
        , [ 0, 1, 1, 0 ] ]

    -- | The figure for a still loaf
    loaf :: Figure
    loaf = toFigure
        [ [ 0, 1, 1, 0 ]
        , [ 1, 0, 0, 1 ]
        , [ 0, 1, 0, 1 ]
        , [ 0, 0, 1, 0 ] ]

    -- | The figure for a still boat
    boat :: Figure
    boat = toFigure
        [ [ 1, 1, 0 ]
        , [ 1, 0, 1 ]
        , [ 0, 1, 0 ] ]

    -- | The figure for a still tub
    tub :: Figure
    tub = toFigure
        [ [ 0, 1, 0 ]
        , [ 1, 0, 1 ]
        , [ 0, 1, 0 ] ]

    
    ---------------------------------------------
    -- Oscillating
    ---------------------------------------------

    -- | The figure for an oscillating blinker
    blinker :: Figure
    blinker = toFigure
        [ [ 1, 1, 1 ] ]

    -- | The figure for an oscillating toad
    toad :: Figure
    toad = toFigure
        [ [ 0, 1, 1, 1 ]
        , [ 1, 1, 1, 0 ] ]

    -- | The figure for an oscillating beacon
    beacon :: Figure
    beacon = toFigure
        [ [ 1, 1, 0, 0 ]
        , [ 1, 1, 0, 0 ]
        , [ 0, 0, 1, 1 ]
        , [ 0, 0, 1, 1 ] ]

    -- | The figure for an oscillating pulsar
    pulsar :: Figure
    pulsar = toFigure
        [ [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1 ]
        , [ 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0 ]
        , [ 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ] ]

    -- | The figure for an oscillating penthadecathlon
    pentadecathlon :: Figure
    pentadecathlon = toFigure
        [ [ 0, 1, 0 ]
        , [ 0, 1, 0 ]
        , [ 1, 1, 1 ]
        , [ 0, 0, 0 ]
        , [ 0, 0, 0 ]
        , [ 1, 1, 1 ]
        , [ 0, 1, 0 ]
        , [ 0, 1, 0 ]
        , [ 0, 1, 0 ]
        , [ 0, 1, 0 ]
        , [ 1, 1, 1 ]
        , [ 0, 0, 0 ]
        , [ 0, 0, 0 ]
        , [ 1, 1, 1 ]
        , [ 0, 1, 0 ]
        , [ 0, 1, 0 ] ]


    ---------------------------------------------
    -- Spaceship
    ---------------------------------------------

    -- | The figure for a glider
    glider :: Figure
    glider = toFigure
        [ [ 0, 1, 0 ]
        , [ 0, 0, 1 ]
        , [ 1, 1, 1 ] ]

    -- | The figure for a light weight spaceship
    lightWeight :: Figure
    lightWeight = toFigure
        [ [ 1, 0, 0, 1, 0 ]
        , [ 0, 0, 0, 0, 1 ]
        , [ 1, 0, 0, 0, 1 ]
        , [ 0, 1, 1, 1, 1 ] ]

    -- | The figure for a middle weight spaceship
    middleWeight :: Figure
    middleWeight = toFigure
        [ [ 0, 1, 1, 1, 1, 1 ]
        , [ 1, 0, 0, 0, 0, 1 ]
        , [ 0, 0, 0, 0, 0, 1 ]
        , [ 1, 0, 0, 0, 1, 0 ]
        , [ 0, 0, 1, 0, 0, 0 ] ]
        
    -- | The figure for a heavy weight spaceship
    heavyWeight :: Figure
    heavyWeight = toFigure
        [ [ 0, 1, 1, 1, 1, 1, 1 ]
        , [ 1, 0, 0, 0, 0, 0, 1 ]
        , [ 0, 0, 0, 0, 0, 0, 1 ]
        , [ 1, 0, 0, 0, 0, 1, 0 ]
        , [ 0, 0, 1, 1, 0, 0, 0 ] ]


    ---------------------------------------------
    -- Other
    ---------------------------------------------

    -- | The figure for a clock
    clock :: Figure
    clock = toFigure
        [ [ 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0 ]
        , [ 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0 ]
        , [ 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0 ]
        , [ 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1 ]
        , [ 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1 ]
        , [ 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0 ] ]

    -- | The figure for a flipped U on top of a U
    doubleU :: Figure
    doubleU = toFigure
        [ [ 1, 1, 1 ]
        , [ 1, 0, 1 ]
        , [ 1, 0, 1 ]
        , [ 0, 0, 0 ]
        , [ 1, 0, 1 ]
        , [ 1, 0, 1 ]
        , [ 1, 1, 1 ] ]
    
    rPentomino :: Figure
    rPentomino = toFigure
        [ [ 0, 1, 1 ]
        , [ 1, 1, 0 ]
        , [ 0, 1, 0 ] ]
    
    -- | The figure for a row of 'block's
    blockRow :: Figure
    blockRow = toFigure
        [ [ 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0 ,1, 1 ]
        , [ 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1 ] ]

    -- | The figure for a little swimmer
    littleSwimmer :: Figure
    littleSwimmer = toFigure
        [ [ 1, 0, 0 ]
        , [ 1, 1, 1 ]
        , [ 0, 1, 1 ]
        , [ 1, 1, 1 ]
        , [ 1, 0, 0 ] ]       

    -- | The figure for a swimmer
    swimmer :: Figure
    swimmer = toFigure
        [ [ 0, 1, 0, 0 ]
        , [ 1, 1, 1, 1 ]
        , [ 0, 0, 1, 1 ]
        , [ 1, 1, 1, 1 ]
        , [ 0, 1, 0, 0 ] ]

    -- | The figure for a wich stretcher
    wichStretcher :: Figure
    wichStretcher = toFigure
        [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1 ]
        , [ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0 ]
        , [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1 ]
        , [ 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0 ]
        , [ 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] ]

    -- | The figure of the left generator of glider gun 1
    gliderGun1Left :: Figure
    gliderGun1Left = toFigure
        [ [ 0, 0, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 1, 0, 0, 0, 1, 0, 0 ]
        , [ 1, 0, 0, 0, 0, 0, 1, 0 ]
        , [ 1, 0, 0, 0, 1, 0, 1, 1 ]
        , [ 1, 0, 0, 0, 0, 0, 1, 0 ]
        , [ 0, 1, 0, 0, 0, 1, 0, 0 ]
        , [ 0, 0, 1, 1, 0, 0, 0, 0 ] ]

    -- | The figure of the right generator of glider gun 1
    gliderGun1Right :: Figure
    gliderGun1Right = toFigure
        [ [ 0, 0, 0, 0, 1 ]
        , [ 0, 0, 1, 0, 1 ]
        , [ 1, 1, 0, 0, 0 ]
        , [ 1, 1, 0, 0, 0 ]
        , [ 1, 1, 0, 0, 0 ]
        , [ 0, 0, 1, 0, 1 ]
        , [ 0, 0, 0, 0, 1 ] ]

    -- | The figure of the top generator of glider gun 2
    gliderGun2Top :: Figure
    gliderGun2Top = toFigure
        [ [ 0, 1, 1, 0, 1, 1, 0, 0 ]
        , [ 1, 0, 0, 0, 0, 0, 1, 0 ]
        , [ 1, 0, 0, 0, 0, 0, 0, 1 ]
        , [ 1, 1, 1, 0, 0, 0, 1, 0 ]
        , [ 0, 0, 0, 0, 0, 1, 0, 0 ] ]

    -- | The figure of the bottom generator of glider gun 2
    gliderGun2Bottom :: Figure
    gliderGun2Bottom = toFigure
        [ [ 1, 1, 0, 0]
        , [ 1, 0, 0, 0]
        , [ 0, 1, 1, 1]
        , [ 0, 0, 0, 1] ]