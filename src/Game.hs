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


import Enemy (chase)
import Datas


type Inventory = Map Item Int

data Action = Move | Fire | Inv
  deriving (Eq, Show)


  
instance Show Object where
  show Player = "@"
  show Enemy = "><"
  show Obstacle = "X"
  show Goal = "F"
  show (Projectile _) = "*"
  show (Collectable x) = show x 
  show Vacant = "-"
  show Boom = "BOOM"

data End = Won | Lost | Ongoing 
  deriving (Eq, Show)


data Stream a = a :| Stream a
  deriving (Show)

data Game = Game
  { _player :: Loc
  , _stage :: Stage
  , _stageDim :: Loc
  , _done  :: End
  , _inventory :: Inventory
  , _action :: Action

  } deriving (Eq)

data MobileEntity = IsPlayer | IsEnemy | IsProjectile

makeLenses ''Game

--make a level with some dimensions with all vacant squares 

emptyStage :: (Int, Int) -> Stage
emptyStage (x,y) = foldr (flip Map.insert Vacant) Map.empty [(x',y') | x' <- [0..x-1], y'<- [0..y-1]]

getAtStage :: Loc -> Stage -> Maybe Object
getAtStage l s = s ^. (at l)

insertObject :: Object -> Loc -> Stage -> Stage
insertObject o p s = s & ix p .~ o

--game locationToMoveTo isPlayer
canMoveHere :: Stage -> Loc -> MobileEntity -> Bool
canMoveHere s l me = case (getAtStage l s , me) of
  (Just Player , _ )-> True
  (Just Enemy , IsEnemy) -> False
  (Just Enemy , _) -> True
  (Just Obstacle , _) -> False
  (Just Goal , IsEnemy) -> False
  (Just Goal , _) -> True
  (Just Vacant , _) -> True
  (Just Boom, _) -> True
  (Just (Projectile _), _) -> True
  (Nothing , _)-> False

nudge :: Loc -> Direction -> Loc
nudge (x,y) d = case d of
  Stay -> (x,y)
  North -> (x, y-1)
  South -> (x, y+1)
  East -> (x+1,y)
  West -> (x-1,y)

--TODO see if to lethal/victorious square 
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
  Fire -> g & action .~ Inv
  Inv -> g & action .~ Move

setAction :: Action -> Game -> Game
setAction a g = g & action .~ a
  

  --chase should be defined in own Module
moveEnemy :: Loc -> Loc -> Stage -> Stage
moveEnemy enemy player s = case (Map.lookup goto s, canMoveHere s goto IsEnemy) of
  (Just Player, True) -> insert goto Boom $ insert enemy Vacant s
  (Just (Projectile _), True) -> insert goto Boom $ insert enemy Vacant s
  (Just Vacant, True) -> insert goto Enemy $ insert enemy Vacant s
  (_, False) -> s
  where goto = nudge enemy (chase enemy player s)

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

playerAction :: Action -> Direction -> Game -> Game
playerAction Fire d g = if fire d g == Nothing then g else (fromJust $ fire d g) & stage %~ moveEnemies (g ^. player)
                                                                                 & stage %~ moveProjectiles
playerAction Move d g = if movePlayer d g == Nothing then g else moveNPE (fromJust $ movePlayer d g)

--spawnEntity :: Object -> Game -> Game
--spawnEntity o g = g & stage %~ insert (head spawn) o
--                    & spawn %~ fromJust . snd . uncons

sampleGame :: Game
sampleGame = Game {
    _player = (1,2)
  , _stage = insertObject Obstacle (8,7) $ insertObject Obstacle (7,7) $ insertObject Obstacle (6,7) $ insertObject Obstacle (5,7) $ insertObject Obstacle (4,7) $ insertObject Obstacle (3,4) $ insertObject Enemy (5,5) $ insertObject Goal (1,3) $ insertObject Player (1,2) (emptyStage (10,10))
  , _stageDim = (10,10)
  , _done = Ongoing
  , _inventory = addItems Bullet 999 Map.empty 
  , _action = Move
--  , _spawn = singleton (-1,-1)
  }

initGame :: IO Game
initGame = do
--  (o :| os) <- fromList . randomRs (V2 0 0, V2 (fst (sampleGame ^. stageDim) -1) (snd(sampleGame ^. stageDim) -1)) <$> newStdGen 
  return $ sampleGame -- & spawn .~ os



main = do
  putStrLn "Hello"
