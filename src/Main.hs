
-- | The main module
module Main where
    import System.Random
    import GameOfLife
    import Graphics.Gloss

    -- | Entry Point for the game
    main :: IO ()
    main = do
        { seed <- randomIO :: IO Int
        ; play window backgroundColor fps
            (gameData seed) drawGame handleKeys update }
        where
            gameData seed = (blankGame $ mkStdGen seed)
            window = (InWindow "Game of Life" windowSize (50, 50))
            fps = 2