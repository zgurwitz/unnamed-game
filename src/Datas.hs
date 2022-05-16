module Datas where
import qualified Data.Map as Map
import Control.Lens hiding ((<|), (|>))
import Control.Lens.TH
import System.Random
type Loc = (Int, Int)
type Stage = Map.Map Loc Object

data Object = Player | Enemy | Obstacle | Goal | Projectile Direction | Collectable Item | Vacant | Boom
  deriving (Eq)
  
instance Show Object where
  show Player = "@"
  show Enemy = "e"
  show Obstacle = "x"
  show Goal = "f"
  show (Projectile _) = "*"
  show (Collectable x) = show x 
  show Vacant = "-"
  show Boom = "o"

data Direction = North | South | East | West | Stay
  deriving (Eq, Show)

data Item = Bullet | Life | Speed | Teleport
  deriving (Eq, Ord)

instance Show Item where
  show Bullet = "0"
  show Life = "&"
  show Speed = ">"
  show Teleport = "t"
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
  (Just (Collectable _), IsPlayer) -> True
  (Just (Collectable _), _) -> False
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

oppo :: Direction -> Direction
oppo North = South
oppo South = North
oppo East = West
oppo West = East
oppo Stay = Stay

randomLoc :: Loc -> IO Loc
randomLoc (a,b) = do
  x <- newStdGen
  y <- newStdGen
  return (head (randomRs (0,a-1) x), head (randomRs (0,b-1) y))

randomLegalLoc :: Stage -> Loc -> IO Loc
randomLegalLoc s l = do
  loc <- randomLoc l
  case getAtStage loc s of
    Nothing -> return loc
    Just Vacant -> return loc
    otherwise -> randomLegalLoc s l

randomLegalLocs :: Int -> Stage -> Loc -> [IO Loc]
randomLegalLocs 0 _ _ = []
randomLegalLocs n s l = (randomLegalLoc s l):randomLegalLocs (n-1) s l
