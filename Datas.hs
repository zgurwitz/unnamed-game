module Datas where
import qualified Data.Map as Map
type Loc = (Int, Int)
type Stage = Map.Map Loc Object

data Object = Player | Enemy | Obstacle | Goal | Projectile Direction | Collectable Item | Vacant | Boom
  deriving (Eq)

data Direction = North | South | East | West | Stay
  deriving (Eq, Show)

data Item = Bullet | Life | Speed | Teleport
  deriving (Eq, Show, Ord)
