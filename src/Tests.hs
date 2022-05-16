module Tests where

import Game
import UI
import Datas
import Enemy
import Search
import Test.QuickCheck
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Map


instance Arbitrary Game where
    arbitrary = sized game where
        game 0 = do
            sx <- choose (3,3)
            sy <- choose (1,1)
            px <- choose (0,sx-1)
            py <- choose (0,sy-1)
            gx <- choose (0,sx-1)
            gy <- choose (0,sy-1)
            let stage = insertObject Player (px,py) $ insertObject Goal (gx,gy) (emptyStage (sx,sy))
            return $ Game (px,py) (gx,gy) stage (sx,sy) Ongoing (insert Bullet (2) Data.Map.empty) Move 0
        game n = do
            g' <- game (n-1)
            let (sx,sy) = g' ^. stageDim
            ox <- choose (0,sx-1)
            oy <- choose (0,sy-1) 
            return $ g' & stage %~ if getAtStage (sx,sy) (g' ^. stage) == Just Vacant then insertObject Obstacle (ox,oy) else id



--is the player in the spot the game says it is
propPlayerCorrect :: Game -> Bool
propPlayerCorrect g
    | g ^. done == Ongoing = getAtStage (g ^. player) (g ^. stage) == Just Player
    | otherwise = True
--is the player in the spot the game says it is
propGoalCorrect :: Game -> Bool
propGoalCorrect g  
    | g ^. player == g ^. goal = True
    | g ^. done == Ongoing = getAtStage (g ^. goal) (g ^. stage) == Just Goal
    | otherwise = True
--is there a path from the player to the goal
propNotSoftLocked :: Game -> Bool
propNotSoftLocked g 
    | g ^. player == g ^. goal = True
    | g ^. done == Ongoing = not $ Search.gBFS (g ^. stage) (g ^. player) (g ^. goal) IsPlayer == Nothing || Search.gBFS (g ^. stage) (g ^. player) (g ^. goal) IsPlayer == Just Stay
    | otherwise = True


shouldFail :: Game
shouldFail = Game (0,0) (0,2) (insertObject Obstacle (0,1) $ insertObject Player (0,0) $ insertObject Goal (0,2) (emptyStage (1,3))) (1,3) Ongoing Data.Map.empty Move 0
