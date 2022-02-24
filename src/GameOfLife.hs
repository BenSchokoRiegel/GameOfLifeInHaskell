
-- | Contains everything related to Conway's Game of Life.
module GameOfLife where

    import Graphics.Gloss
    import System.Random
    import Control.Lens
    import Graphics.Gloss.Data.ViewPort
    import Graphics.Gloss.Interface.IO.Interact
    import DataTypes
    import Figures
    import Graphics.Gloss.Data.Color
    

    -- | The color of the background
    backgroundColor :: Color
    backgroundColor = black

    -- | The color of the cells
    cellColor :: Color
    cellColor = white

    -- | The color of the grid
    gridColor :: Color
    gridColor = greyN 0.5

    -- | Generates a random grid of cells
    genRandomCells ::
        StdGen -- ^ The random generator
        -> Grid -- ^ The resulting random grid
    genRandomCells stdGen
        = take (cellsH * cellsV)
            [ val | rand <- randoms stdGen :: [Bool],
                    let val = if rand then Alive else Dead]
        where (cellsH, cellsV) = cellCount
    
    -- | Create a new game with a random field
    randomGame ::
        StdGen -- ^ The random generator
        -> GameData -- ^ The new game state
    randomGame stdGen = newGameData stdGen (genRandomCells stdGen) white
    
    -- | Create a new game with a blank field
    blankGame ::
        StdGen -- ^ The random generator
        -> GameData -- ^ The new game state
    blankGame stdGen = newGameData stdGen [cell | i <- [1..(cellsW*cellsH)],
                                                  let cell = Dead ] white
        where (cellsW, cellsH) = cellCount
    
    -- | Creates a new game state.
    newGameData ::
        StdGen -- ^ The random generator
        -> Grid -- ^ The starting grid
        -> Color -- ^ The cell color
        -> GameData -- ^ The resultung game state
    newGameData stdGen grid color = GameData {
        grid = grid,
        startGrid = grid,
        running = False,
        step = False,
        cColor = color,
        showGrid = False,
        nextFigure = None,
        stdGen = stdGen
    }

    -- | The initial window size
    windowSize :: (Int, Int)
    windowSize = (700, 500)

    -- | The size in pixels of a single 'Cell'
    cellSize :: Int
    cellSize = 15

    -- | The cell count in x and y direction
    cellCount :: (Int, Int)
    cellCount = (windowW `div` cellSize, windowH `div` cellSize)
        where
            (windowW, windowH) = windowSize

    -- | Draws a single 'Cell'
    drawCell ::
        Cell -- ^ The cell to draw
        -- | The position of the cell, in cell-coordinates rather than pixels
        -> Position
        -> Color -- ^ The cell color
        -> Picture -- ^ A function to draw the wiregrid
        -> Picture -- ^ The resulting picture
    drawCell cell pos col wire = case cell of
        Dead  -> translatePosition pos $ wire
        Alive -> translatePosition pos
                $ color col
                $ pictures
                    [ circleSolid (fromIntegral $ cellSize `div` 2)
                    , wire
                    ]

    -- | Translates a 'Picture' using a cells 'Position'
    translatePosition ::
        Position -- ^ The cell position in cell-coordinate
        -> Picture -- ^ The old picture
        -> Picture -- ^ The resulting picture, after the translation
    translatePosition (x, y) = translate
        ( fromIntegral
            $ -windowW `div` 2 + x * cellSize + cellSize `div` 2 )
        ( fromIntegral
            $  windowH `div` 2 - y * cellSize - cellSize `div` 2 )
        where
            (windowW, windowH) = windowSize

    -- | Draws the wire background.
    drawWire ::
        Color -- ^ The wire color
        -> Bool -- ^ Whether to draw the wire or not
        -> Picture -- ^ The resulting picture
    drawWire col show = case show of
        True -> color col
                    $ rectangleWire (fromIntegral cellSize)
                                    (fromIntegral cellSize)
        _    -> blank

    -- | Draws the given game state.
    drawGame ::
        GameData -- ^ The game state
        -> Picture -- ^ The resulting picture
    drawGame gameData = pictures
        [ drawGrid 0 (grid gameData)
                   (cColor gameData) gridColor (showGrid gameData)
        , translate (fromIntegral $ -width `div` 2 + 10)
                    (fromIntegral $ -height `div` 2 + 10)
            $ scale 0.2 0.2
            $ color white
            $ text
            $ show (nextFigure gameData)
        ]
        where
            (width, height) = windowSize

    -- | Draws a 'Grid'
    drawGrid ::
        Int -- ^ The current cell index in the grid
        -> Grid -- ^ The 'Grid' to draw
        -> Color -- ^ The cell color
        -> Color -- ^ The grid color
        -> Bool -- ^ Whether to show the wiregrid
        -> Picture -- ^ The resulting 'Picture'
    drawGrid _ [] _ _ _ = blank
    drawGrid index (cell : rest) cellCol wCol showGrid = pictures
        [ drawCell cell (x, y) cellCol $ drawWire wCol showGrid
        , drawGrid (index + 1) rest cellCol wCol showGrid
        ]
        where
            x = index `mod` cellsW
            y = index `div` cellsW
            (cellsW, _) = cellCount

    -- | Inserts the glider gun 2 into a given 'Grid' at a 'Position'
    insertGliderGun1 ::
        Grid -- ^ The grid to insert the glider gun into
        -> Position -- ^ The position to insert at
        -> Grid -- ^ The resulting grid, after the insertion
    insertGliderGun1 grid (x, y) = grid4
        where
            grid1 = insertFigure grid 0 (x, y+4) block
            grid2 = insertFigure grid1 0 (x+10, y+2) gliderGun1Left
            grid3 = insertFigure grid2 0 (x+20, y) gliderGun1Right
            grid4 = insertFigure grid3 0 (x+34, y+2) block

    -- | Inserts the glider gun 1 into a given 'Grid' at a 'Position'
    insertGliderGun2 ::
        Grid -- ^ The grid to insert the glider gun into
        -> Position -- ^ The position to insert at
        -> Grid -- ^ The resulting grid, after the insertion
    insertGliderGun2 grid (x, y) = grid6
        where
            grid1 = insertFigure grid 0 (x, y) block
            grid2 = insertFigure grid1 0 (x+7, y) block
            grid3 = insertFigure grid2 0 (x+4, y+3) block
            grid4 = insertFigure grid3 0 (x+31, y+11) block
            grid5 = insertFigure grid4 0 (x+21, y+9) gliderGun2Top
            grid6 = insertFigure grid5 0 (x+20, y+17) gliderGun2Bottom


    -- | Returns the 'Cell' at a given 'Position' in a given 'Grid'.
    getCellAtPos ::
        Grid -- ^ The 'Grid' the cell is in
        -> Position -- ^ The 'Position' the cell is at
        -> Cell -- ^ The resulting cell
    getCellAtPos grid (x, y)
        | (x < 0 || y < 0 || x >= cellsW || y >= cellsH) = Dead
        | otherwise = grid ^?! element index -- Bsp: [1,2,3] ^?! element 1 = 2
        where
            (cellsW, cellsH) = cellCount
            index = x + cellsW * y

    -- | Simulates a 'Cell' with Conway's rules.
    --
    -- These are the rules:
    --
    --      * An 'Alive' cell lives with 2 or 3 alive neighbors
    --      * A 'Dead' cell lives with exactly 3 alive neighbors
    --      * A cell with less than 2 or more than 3 neighbors dies
    simulateCell ::
        Cell -- ^ The cell to simulate
        -> Int -- ^ The number of alive neighbors
        -> Cell -- ^ The resulting cell, after the simulation
    simulateCell cell neighbors = case (cell, neighbors) of
        (Alive, 2) -> Alive
        (_, 3) -> Alive
        _ -> Dead

    -- | Simulates a 'Grid' with Conway's rules.
    --
    -- For exact rules see 'simulateCell'
    simulateGrid ::
        Grid -- ^ The grid to simulate
        -> Int -- ^ The index of the current cell
        -> Grid -- ^ The resuling grid, after the simulation
    simulateGrid grid i
        | i >= (cellsW * cellsH) = []
        | otherwise = simulateCell cell neighbors : simulateGrid grid (i+1)
        where
            cell = getCellAtPos grid (x, y)
            neighbors = countAliveNeighbors (x, y) grid
            x = i `mod` cellsW
            y = i `div` cellsW
            (cellsW, cellsH) = cellCount

    -- | Counts a single 'Cell'.
    countCell ::
        Cell -- ^ The cell to count
        -- | The resulting number. 1 if the cell is 'Alive', 0 otherwise.
        -> Int
    countCell cell = case cell of
        Alive -> 1
        Dead -> 0

    -- | Returns the number of neighboring cells that are 'Alive'.
    countAliveNeighbors ::
        Position -- ^ The position the cell is at
        -> Grid -- ^ The grid the cell is in
        -> Int -- ^ The amount of alive neighbors
    countAliveNeighbors (x, y) grid =
        countCell (getCellAtPos grid topLeft) +
        countCell (getCellAtPos grid topMid) +
        countCell (getCellAtPos grid topRight) +
        countCell (getCellAtPos grid midLeft) +
        countCell (getCellAtPos grid midRight) +
        countCell (getCellAtPos grid bottomLeft) +
        countCell (getCellAtPos grid bottomMid) +
        countCell (getCellAtPos grid bottomRight)
        where
            topLeft = (x-1, y-1)
            topMid = (x, y-1)
            topRight = (x+1, y-1)
            midLeft = (x-1, y)
            midRight = (x+1, y)
            bottomLeft = (x-1, y+1)
            bottomMid = (x, y+1)
            bottomRight = (x+1, y+1)


    -- | Inserts a 'Figure' at a given 'Grid' and position
    insertFigure ::
        Grid -- ^ The grid to insert the figure into
        -> Int -- ^ The index we are at
        -> Position -- ^ The position to insert the figure
        -> Figure -- ^ The figure to insert
        -> Grid -- ^ The resulting grid, after the insertion
    insertFigure [] _ _ _ = []
    insertFigure grid _ _ [] = grid
    insertFigure (cell : grid) index (x, y) (part : fig)
        | index < targetIndex
            = cell : insertFigure grid (index + 1) (x, y) (part : fig)
        | otherwise
            = insertedCells ++ insertFigure grid2
                (index + insertedCount) (x, y + 1) fig
        where
            targetIndex = y*cellsW + x
            (cellsW, _) = cellCount
            (grid2, insertedCells, insertedCount)
                = insertFigurePart (cell : grid) [] 0 part

    -- | Inserts a 'FigurePart' at a given 'Grid' and index
    insertFigurePart ::
        Grid -- ^ The grid to insert the part into
        -> [Cell] -- ^ The cells of that part that were already inserted
        -> Int -- ^ The index in the grid to insert this part
        -> FigurePart -- ^ The part to insert
        -- | The result: (rest of the grid after inserting,
        -- the inserted cells, the number of inserted cells)
        -> (Grid, [Cell], Int)
    insertFigurePart [] iCells count _ = ([], iCells, count)
    insertFigurePart grid iCells count [] = (grid, iCells, count)
    insertFigurePart (_ : grid) iCells count (cell : part)
        = insertFigurePart grid (iCells ++ [cell]) (count + 1) part


    insertAtPos :: Cell -> Int -> Position -> Grid -> Grid
    insertAtPos cell index (x,y) (g:gs) 
        | index < targetIndex
            = [g] ++ insertAtPos cell (index + 1) (x, y) gs
        | otherwise
            = [cell] ++ gs
        where targetIndex = y*cellsW + x
              (cellsW, _) = cellCount
    -- | Returns the subsequent color to the specified color.
    --
    -- This is used to change the color of the cells.
    --
    -- The order is the following:
    --
    --      * white
    --      * red
    --      * green
    --      * blue
    --      * yellow
    --      * cyan
    --      * magenta
    --      * rose
    --      * violet
    --      * orange
    nextColor ::
        Color -- ^ The previous color
        -> Color -- ^ The next color
    nextColor col
        | col == white   = red
        | col == red     = green
        | col == green   = blue
        | col == blue    = yellow
        | col == yellow  = cyan
        | col == cyan    = magenta
        | col == magenta = rose
        | col == rose    = violet
        | col == violet  = orange
        | otherwise      = white


-- toDO Save Grid for Reset  --

    -- | Updates the game state depending on pressed keys.
    -- The following are interactable keys:
    --
    --      * Space -> Pause and Unpause the game
    --      * Right Arrow -> Step one iteration forward
    --      * F1 -> Clear grid
    --      * F2 -> Create a random grid
    --      * F12 -> Change cell color (see 'nextColor')
    --      * R -> Reset grid
    --      * G -> Show / Hide grid
    --      * N -> Select next figure to place
    --      * B -> go back at next figure to place
    --      * LeftMouseButton -> Place selected figure at cursor
    handleKeys ::
        Event -- ^ The key event that has occured
        -> GameData -- ^ The old game state
        -> GameData -- ^ The new game state, after the key press was handled
    handleKeys (EventKey key Down _ pos) gameData = case key of
        (SpecialKey KeySpace)    -> gameData
            { running = not (running gameData) }
        (SpecialKey KeyRight)    -> gameData
            { step = True }
        (SpecialKey KeyF1)       -> blankGame (stdGen gameData)
        (SpecialKey KeyF2)       -> randomGame (stdGen gameData)
        (SpecialKey KeyF12)      -> gameData
            { cColor = nextColor (cColor gameData) }
        (Char 'r')               -> gameData
            { grid = (startGrid gameData)
            , running = False
            , step = False }
        (Char 'n')               -> gameData
            { nextFigure = nextFig (nextFigure gameData) }
        (Char 'b')               -> gameData
            { nextFigure = backFig (nextFigure gameData) }    
        (Char 'g')               -> gameData
            { showGrid = not (showGrid gameData) }
        (Char 'a')               -> gameData
            { nextFigure = Revive }
        (Char 'd')               -> gameData
            { nextFigure = Kill }
        (Char 's')   -> gameData
            {startGrid = (grid gameData) }         
        (MouseButton LeftButton) -> case (nextFigure gameData) of
                Revive -> gameData 
                    { grid = insertAtPos Alive 0 (pixelPosToCellPos pos) (grid gameData)  }
                Kill -> gameData 
                    { grid = insertAtPos Dead 0 (pixelPosToCellPos pos) (grid gameData)  }    
                GliderGun1 -> gameData
                    { grid = insertGliderGun1
                             (grid gameData)
                             (pixelPosToCellPos pos) }
                GliderGun2 -> gameData
                    { grid = insertGliderGun2
                             (grid gameData)
                             (pixelPosToCellPos pos) }
                _ -> gameData
                    { grid = insertFigure
                             (grid gameData) 0
                             (pixelPosToCellPos pos)
                             (figureForType (nextFigure gameData)) }
        _ -> gameData
    handleKeys _ gameData = gameData

    -- | Converts pixel coordinates into cell coordinates
    --
    -- The cell coordinates start with (0, 0) at the top left corner
    -- but pixel coordinates start at the center of the window
    pixelPosToCellPos ::
        (Float, Float) -- ^ The pixel coordinates
        -> Position -- ^ The resulting cell coordinates
    pixelPosToCellPos (x, y) = (transX `div` cellSize, transY `div` cellSize)
        where
            (transX, transY) = (floor $ x + halfW, floor $ - y + halfH)
            (width, height) = windowSize
            (halfW, halfH) =
                ( fromIntegral (width `div` 2)
                , fromIntegral (height `div` 2) )
    

    -- | Steps the game state one iteration forward.
    update :: 
        -- | The time in seconds that has passed since the last call
        -- to this function
        Float
        -> GameData -- ^ The old game state
        -> GameData -- ^ The new game state, after stepping one iteration
    update _ oldData
        | step oldData = oldData
            { grid = (simulateGrid (grid oldData) 0)
            , step = False
            }
        | running oldData = oldData
            { grid = (simulateGrid (grid oldData) 0) }
        | otherwise = oldData