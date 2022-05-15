module Datas where
import qualified Data.Map as Map
import Control.Lens hiding ((<|), (|>))
import Control.Lens.TH

type Loc = (Int, Int)
type Stage = Map.Map Loc Object

data Object = Player | Enemy | Obstacle | Goal | Projectile Direction | Collectable Item | Vacant | Boom
  deriving (Eq)

data Direction = North | South | East | West | Stay
  deriving (Eq, Show)

data Item = Bullet | Life | Speed | Teleport
  deriving (Eq, Show, Ord)

data MobileEntity = IsPlayer | IsEnemy | IsProjectile

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


getAtStage :: Loc -> Stage -> Maybe Object
getAtStage l s = s ^. (at l)

nudge :: Loc -> Direction -> Loc
nudge (x,y) d = case d of
  Stay -> (x,y)
  North -> (x, y-1)
  South -> (x, y+1)
  East -> (x+1,y)
  West -> (x-1,y)
