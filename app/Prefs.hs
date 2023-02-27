module Prefs where

------ Adjustable preferences for game ------

gameTitle :: [Char]
gameTitle = "Haskellvaders"

refreshRateHz :: Int
refreshRateHz = 60

pageWidth :: Int -- pixels
pageWidth = 500

pageHeight :: Int -- pixels
pageHeight = 500

playerSize :: Float
playerSize = 10 -- side-length of player square box in pixels

-- laserSize is purely visual, laser collisions with enemies are calculated at the top pixel of the laser
laserSize :: Float -- side-length of laser square box in pixels
laserSize = 5

laserSpeed :: Int -- pixels to translate per frame
laserSpeed = 15

playerSpeed :: Int -- pixels to translate per movement
playerSpeed = 15

-- note: collision boxes are designed for square enemies, where enemiesHorizontalCount = enemiesVerticalCount 
enemiesHorizontalCount :: Int
enemiesVerticalCount :: Int
enemiesHorizontalCount = 10
enemiesVerticalCount = 10

enemyHitsUntilDeath :: Int
enemyHitsUntilDeath = 5

enemyPositionPaddingTopPercent :: Int
enemyPositionPaddingTopPercent = 5

enemyPositionPaddingBottomPercent :: Int
enemyPositionPaddingBottomPercent = 5

enemyPositionPaddingSidesPercent :: Int -- per side
enemyPositionPaddingSidesPercent = 5




