module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Prefs
import TerritoryCalculations

-- Data Types - Start
data World = World {
     player :: Player,
     enemies :: [Enemy],
     lasers :: [Laser]
} deriving (Show)

data Player = Player {
     playerPosition :: (Int, Int)
} deriving (Show)

data Laser = Laser {
     laserPosition :: (Int, Int)
} deriving (Show, Eq)

data Enemy = Enemy {
     enemyPosition :: (Int, Int),
     enemyHitsIncurred :: Int,
     enemyAlive :: Bool
} deriving (Show, Eq)
-- Data Types - End

-- Initializing the world
initWorld :: World
initWorld = World initPlayer initEnemies []

-- Initializing player at the bottom center of the world
initPlayer :: Player
initPlayer = Player {
          playerPosition = (div pageWidth 2, -5 * (div pageHeight 10))
}

-- Initializing enemies at equidistantly depending on the number of enemies preferred (based on Prefs.hs)
initEnemies :: [Enemy]
initEnemies = map (\e -> Enemy ((enemyPositionPaddingSides + (2 * enemyTerritoryHorizontalPixelsPerSlot * fst e)),
                               (enemyPositionPaddingTop + (2 * enemyTerritoryVerticalPixelsPerSlot * snd e)))
                               0
                               True)
                  enemyPositions
                         -- columnNum reflects horizontal; vice-versa
                        where enemyPositions = [(columnNum, rowNum) | rowNum <- [0..(enemiesVerticalCount-1)], 
                                                                    columnNum <- [0..(enemiesHorizontalCount-1)]]

-- Determines if laser is intersecting enemies with threshold (assumes square enemy)
isColliding :: Enemy -> Laser -> Bool
isColliding enemy laser = threshold >= dist (laserPosition laser) (enemyPosition enemy)
            where dist (x1, y1) (x2, y2) = sqrt ((fromIntegral (x2 - x1))^2 + (fromIntegral (y2 - y1))^2)
                  threshold = sqrt (fromIntegral (enemyTerritoryHorizontalPixelsPerSlot * enemyTerritoryVerticalPixelsPerSlot))

main :: IO ()
main = play
     (InWindow gameTitle (fromIntegral pageWidth, fromIntegral pageHeight) (0,0))
     black
     refreshRateHz
     initWorld
     paint
     eventAction
     update

-- Renders elements on the world
paint :: World -> Picture
paint world = pictures $ [paintPlayer (player world)] -- generates player
                         ++ map paintEnemy (filter enemyAlive (enemies world)) -- generates all enemies
                         ++ map paintEnemyHits (filter enemyAlive (enemies world)) -- generates all enemy status texts
                         ++ map paintLaser (lasers world) -- generates all lasers
                         ++ [paintScore (enemies world)] -- generates score text
                         ++ [paintTitle] -- generates title text
      where
        paintPlayer :: Player -> Picture
        paintPlayer player = translate (fromIntegral x) (fromIntegral y) $ color blue $ rectangleSolid playerSize playerSize
                    where (x,y) = playerPosition player
        paintEnemy :: Enemy -> Picture
        paintEnemy enemy = translate (fromIntegral x) (fromIntegral y) $ color red $ rectangleSolid enemyWidth enemyHeight
                    where (x, y) = enemyPosition enemy
                          enemyWidth = fromIntegral enemyTerritoryHorizontalPixelsPerSlot
                          enemyHeight = fromIntegral enemyTerritoryVerticalPixelsPerSlot
        paintEnemyHits :: Enemy -> Picture
        paintEnemyHits enemy = translate (fromIntegral x) (fromIntegral y) $ (scale 0.1 0.1 (color white (text (show (enemyHitsIncurred enemy)))))
                    where (x, y) = enemyPosition enemy
                          enemyWidth = fromIntegral enemyTerritoryHorizontalPixelsPerSlot
                          enemyHeight = fromIntegral enemyTerritoryVerticalPixelsPerSlot
        paintLaser :: Laser -> Picture
        paintLaser laser = translate (fromIntegral x) (fromIntegral y) $ color white $ rectangleSolid laserSize laserSize
                    where (x, y) = laserPosition laser
        paintScore :: [Enemy] -> Picture
        paintScore enemies = translate (-300) 300 (scale 0.2 0.2 (color white (text ("Score: " ++ show score))))
                   where score = length (filter (\e -> not (enemyAlive e)) enemies)
        paintTitle :: Picture
        paintTitle = translate (-300) (-100) (scale 0.2 0.2 (color white (text gameTitle)))

-- Updates players, enemies, and lasers on every frame
update :: Float -> World -> World
update delta world =
       World {
             player = (player world), -- remains unchanged if idle
             enemies = updateEnemies (enemies world) (lasers world),
             lasers = updateLasers (lasers world)
       }

-- Checks for collisions of enemy with laser; kills enemies who have incurred threshold number of hits
updateEnemies :: [Enemy] -> [Laser] -> [Enemy]
updateEnemies enemies lasers = map (updateEnemy lasers) enemies
              where
                updateEnemy :: [Laser] -> Enemy -> Enemy
                updateEnemy lasers enemy =
                            let colliding_lasers = filter (isColliding enemy) lasers
                                laser_hits = length colliding_lasers
                                enemyNew = enemy { enemyHitsIncurred = laser_hits + enemyHitsIncurred enemy }
                            in
                                if (enemyAlive enemyNew) && ((enemyHitsIncurred enemyNew) >= enemyHitsUntilDeath)
                                then enemyNew {enemyAlive = False}
                                else enemyNew

-- Moves the lasers vertically across the speed by one frame
updateLasers :: [Laser] -> [Laser]
updateLasers lasers = map updateLaser lasers
             where updateLaser :: Laser -> Laser
                   updateLaser laser = laser { laserPosition = ((fst $ laserPosition laser),
                                                               (laserSpeed + (snd $ laserPosition laser))) }

-- User actions (move, shoot, reset)
eventAction :: Event -> World -> World
eventAction (EventKey (SpecialKey KeyRight) Down _ _) world = -- RIGHT key
            world { player = movePlayer (player world) (1) }
eventAction (EventKey (SpecialKey KeyLeft) Down _ _) world = -- LEFT key
            world { player = movePlayer (player world) (-1) }
eventAction (EventKey (SpecialKey KeyUp) Down _ _) world = -- UP key (shoot)
            world { lasers = shootLaser (lasers world) (player world) }
eventAction (EventKey (Char 'r') Down _ _) world = -- R button (reset)
            initWorld
eventAction _ world = world

-- Horizontal movements of the player
movePlayer :: Player -> Int -> Player
movePlayer player multiplier = player { playerPosition = (((multiplier * playerSpeed) + (fst $ playerPosition player)),
                                                         (snd $ playerPosition player)) }

-- Generates a laser to shoot from the player location
shootLaser :: [Laser] -> Player -> [Laser]
shootLaser lasers player = (Laser ((x,y))) : lasers
           where (x,y) = playerPosition player