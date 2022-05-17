{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where
import Control.Applicative ((<|>))
import Control.Monad (guard)

import Control.Lens hiding ((<|), (|>))
import Control.Lens.TH
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Data.List (nub)
import Data.Map (Map, foldrWithKey, insert, lookup)
import qualified Data.Map as Map
import System.Random

import Enemy (chase, chase')
import Datas
import Search


type Inventory = Map Item Int


data Action = Move | Fire | Inv
  deriving (Eq, Show)



data End = Won | Lost | Ongoing 
  deriving (Eq, Show)


data Stream a = a :| Stream a
  deriving (Show)

data Game = Game
  { _player :: Loc
  , _goal :: Loc
  , _stage :: Stage
  , _stageDim :: Loc
  , _done  :: End
  , _inventory :: Inventory
  , _action :: Action
  , _score :: Int

  } deriving (Eq, Show)



makeLenses ''Game

--make a level with some dimensions with all vacant squares 

emptyStage :: (Int, Int) -> Stage
emptyStage (x,y) = foldr (flip Map.insert Vacant) Map.empty [(x',y') | x' <- [0..x-1], y'<- [0..y-1]]

insertObject :: Object -> Loc -> Stage -> Stage
insertObject o p s = s & ix p .~ o


movePlayer :: Direction -> Game -> Maybe Game
movePlayer d g | canMoveHere (g ^. stage) (nudge (g ^. player) d) IsPlayer = case getAtStage (nudge (g ^. player) d) (g ^. stage) of
  Just (Projectile _) -> g & stage %~ insertObject Vacant (g ^. player)
                           & stage %~ insertObject Boom (nudge (g ^. player) d)
                           & player .~ (-1,-1)
                           & done .~ Lost
                           & Just
  Just Enemy ->          g & stage %~ insertObject Vacant (g ^. player)
                           & stage %~ insertObject Boom (nudge (g ^. player) d)
                           & player .~ (-1,-1)
                           & done .~ Lost
                           & Just
  Just (Collectable i) ->g & stage %~ insertObject Vacant (g ^. player)
                           & stage %~ insertObject Player (nudge (g ^. player) d)
                           & player %~ flip nudge d
                           & inventory %~ addItem i
                           & Just
  Just Goal ->           g & stage %~ insertObject Vacant (g ^. player)
                           & stage %~ insertObject Player (nudge (g ^. player) d)
                           & player %~ flip nudge d 
                           & done .~ Won
                           & Just
  
  otherwise ->           g & stage %~ insertObject Vacant (g ^. player)
                           & stage %~ insertObject Player (nudge (g ^. player) d)
                           & player %~ flip nudge d
                           & Just
movePlayer _ _ = Nothing

deleteBooms :: Stage -> Stage
deleteBooms s = foldrWithKey f Map.empty s where
  f k Boom s' = insert k (Collectable Bullet) s'
  f k o s' = insert k o s' 

--TODO see if hits me
fire :: Direction -> Game -> Maybe Game
fire Stay _ = Nothing
fire _ g | useItem Bullet (g ^. inventory) == Nothing = Nothing
fire d g = g & stage %~ insertObject (Projectile d) (nudge (g ^. player) d)
             & inventory %~ fromMaybe Map.empty . useItem Bullet
             & Just 

moveProjectiles :: Stage -> Stage
moveProjectiles s = foldrWithKey f Map.empty s where
  f k (Projectile d) s' = case (Map.lookup k s', Map.lookup (nudge k d) s', Map.lookup (nudge k d) s) of 
    (_, _, Nothing) -> insert k Vacant s'
    (Nothing, Nothing, Just (Projectile _)) -> insert k Vacant $ insert (nudge k d) (Projectile d) s'
    (Nothing, Nothing, Just dest) -> insert k Vacant $ insert (nudge k d) (projCollRes d dest) s'
    (Just _, Nothing, Just dest) -> insert (nudge k d) (projCollRes d dest) s'
    (Just _, Just b, _) -> insert (nudge k d) (projCollRes d b) s'
    (Nothing, Just b, _) -> insert k Vacant $ insert (nudge k d) (projCollRes d b) s'
  f k o s' = case (o, Map.lookup k s') of
    (Vacant, Just (Projectile _)) -> s'
    otherwise -> insert k o s'

projCollRes :: Direction -> Object -> Object
projCollRes d Vacant = Projectile d
projCollRes d Boom = Projectile d
projCollRes _ Obstacle = Obstacle
projCollRes _ Goal = Goal
projCollRes _ _ = Boom
  
isProjectile :: Object -> Bool
isProjectile (Projectile _) = True
isProjectile _ = False

--non-player entities move
moveNPE :: Game -> Game
moveNPE g = g & stage %~ moveProjectiles
              & stage %~ moveEnemies (g ^. player)
              & stage %~ moveProjectiles
              & done %~ case getAtStage (g ^. player) (g ^. stage) of
                          Just Player -> id 
                          otherwise -> const Lost

cycleAction :: Game -> Game
cycleAction g = case g ^. action of
  Move -> g & action .~ Fire
  Fire -> g & action .~ Move
--  Inv -> g & action .~ Move

setAction :: Action -> Game -> Game
setAction a g = g & action .~ a
  

  --chase should be defined in own Module
moveEnemy :: Loc -> Loc -> Stage -> Stage
moveEnemy enemy player s = case (Map.lookup goto s, canMoveHere s goto IsEnemy) of
  (Just Player, True) -> insert goto Boom $ insert enemy Vacant s
  (Just (Projectile _), True) -> insert goto Boom $ insert enemy Vacant s
  (Just _, True) -> insert goto Enemy $ insert enemy Vacant s
  (_, False) -> s
  where goto = nudge enemy (chase' enemy player s)

moveEnemyList :: [Loc] -> Loc -> Stage -> Stage
moveEnemyList (x:xs) player s = moveEnemyList xs player (moveEnemy x player s)
moveEnemyList [] _ s = s

moveEnemies :: Loc -> Stage -> Stage
moveEnemies player s = moveEnemyList (Map.foldrWithKey f [] s) player s where
  f k Enemy l = k:l
  f k _ l = l
  
useItem :: Item -> Inventory -> Maybe Inventory
useItem i inv | inv ^. at i . non 0 > 0 = 
   inv & ix i %~ (+ (-1))
       & Just
useItem _ _ = Nothing

addItem :: Item -> Inventory -> Inventory
addItem i inv = addItems i 1 inv

addItems :: Item -> Int -> Inventory -> Inventory
addItems i n inv = inv & at i . non 0 %~ (+n)

step :: Game -> Game
step = id

newLevel :: Game -> End -> IO Game
newLevel g Won = do
  g' <- randomGame
  return $ g' & score .~ ((g ^. score) + 10)
newLevel g _ = randomGame
 
checkAliveAfterAction :: Action -> Direction -> Game -> Game
checkAliveAfterAction a d g = if not (getAtStage (g' ^. player) (g' ^. stage) == Just Player) 
  then g' & player .~ (-1,-1)
          & done .~ Lost  
  else g' where
    g' = playerAction a d g

playerAction :: Action -> Direction -> Game -> Game
playerAction Fire d g = if fire d g == Nothing then g else (fromJust $ fire d g) & stage %~ deleteBooms
                                                                                 & stage %~ moveEnemies (g ^. player)
                                                                                 & stage %~ moveProjectiles
playerAction Move d g = if movePlayer d g == Nothing then g else moveNPE (fromJust $ movePlayer d g) & stage %~ deleteBooms

showItem :: Inventory -> Item -> String 
showItem inv Bullet 
  | bulletCount == Nothing || bulletCount == Just 0 = []
  | otherwise = '*':showItem (insert Bullet ((fromJust bulletCount) - 1) inv) Bullet
  where bulletCount = Map.lookup Bullet inv 

sampleGame :: Game
sampleGame = Game {
    _player = (1,2)
  , _goal = (1,3)
  , _stage = insertObject Obstacle (8,7) $ insertObject Obstacle (7,7) $ insertObject Obstacle (6,7) $ insertObject Obstacle (5,7) $ insertObject Obstacle (4,7) $ insertObject Obstacle (3,4) $ insertObject Enemy (9,9) $ insertObject Goal (1,3) $ insertObject Player (1,2) (emptyStage (10,10))
  , _stageDim = (10,10)
  , _done = Ongoing
  , _inventory = addItems Bullet 999 Map.empty 
  , _action = Move
  , _score = 0
  }

randomGame :: IO Game
randomGame = do  
  g <- newStdGen
  let xStage = head (randomRs (30,20) g)
  let yStage = head (randomRs (30,20) g)
  p <- randomLoc (xStage, yStage)
  let s = insertObject Player p (emptyStage (xStage, yStage))
  let oNum = head (randomRs (15, div (xStage*yStage) 5) g)  
  obs <- sequence $ randomLegalLocs oNum s (xStage,yStage)
  goal <- randomLegalLoc s (xStage, yStage)
  let eNum = head (randomRs (1,4) g)
  enemies <- sequence $ randomLegalLocs eNum s (xStage,yStage)
  
  return $ Game {
      _player = p
    , _goal = goal
    , _stage = insertObject Goal goal $ foldr (\a b -> insertObject Enemy a b) (foldr (\a b -> insertObsNotSoftLocked a p  goal b) s obs) enemies 
    , _stageDim = (xStage, yStage)
    , _done = Ongoing
    , _inventory = addItems Bullet (eNum + 2) Map.empty
    , _action = Move
    , _score = 0
  } 

insertObsNotSoftLocked :: Loc -> Loc -> Loc -> Stage -> Stage
insertObsNotSoftLocked target player goal s = if not (sch == Nothing || sch == Just Stay)
                           then s' 
                           else s'
                           where s' = insertObject Obstacle target s 
                                 sch = Search.gBFS (s') (player) (goal) IsPlayer 
  


initGame :: IO Game
initGame = do
  randomGame 


