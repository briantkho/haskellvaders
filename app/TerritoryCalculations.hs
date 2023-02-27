module TerritoryCalculations where

import Prefs



------ Territory Calculations based on Prefs ------


-- Padding Calculations - Start
-- Padding is dependent on page size
enemyPositionPaddingTop :: Int
enemyPositionPaddingTop = div (enemyPositionPaddingTopPercent * pageHeight) 100

enemyPositionPaddingBottom :: Int
enemyPositionPaddingBottom = div (enemyPositionPaddingBottomPercent * pageHeight) 100

enemyPositionPaddingSides :: Int
enemyPositionPaddingSides = div (enemyPositionPaddingSidesPercent * pageWidth) 100
-- Padding Calculations - End



-- Enemies available in the enemy territory calculations - Start
enemyPixelsAvailableHorizontal :: Int -- number of horizontal pixels available, given padding
enemyPixelsAvailableHorizontal = pageWidth - (2 * enemyPositionPaddingSides)

enemyPixelsAvailableVertical :: Int -- number of vertical pixels available, given padding
enemyPixelsAvailableVertical = pageHeight - enemyPositionPaddingTop - enemyPositionPaddingBottom

enemyTerritoryNumberOfHorizontalSlotsWithSpacers :: Int -- number of total horizontal "slots" to place enemies or spacers
enemyTerritoryNumberOfHorizontalSlotsWithSpacers = (enemiesHorizontalCount) + (enemiesHorizontalCount - 1)

enemyTerritoryNumberOfVerticalSlotsWithSpacers :: Int -- number of total vertical "slots" to place enemies or spacers
enemyTerritoryNumberOfVerticalSlotsWithSpacers = (enemiesVerticalCount) + (enemiesVerticalCount - 1)

enemyTerritoryHorizontalPixelsPerSlot :: Int -- number of horizontal pixels per slot
enemyTerritoryHorizontalPixelsPerSlot = div enemyPixelsAvailableHorizontal enemyTerritoryNumberOfHorizontalSlotsWithSpacers

enemyTerritoryVerticalPixelsPerSlot :: Int -- number of vertical pixels per slot
enemyTerritoryVerticalPixelsPerSlot = div enemyPixelsAvailableVertical enemyTerritoryNumberOfVerticalSlotsWithSpacers
-- Enemies available in the enemy territory calculations - End