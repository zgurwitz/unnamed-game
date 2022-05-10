module Enemy where

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
import Datas

-- parent loc, f g h
type Cell = ((Int, Int), Int, Int, Int)
type Deets = Map Loc Cell
type Closed = Map Loc Bool

--type Loc = (Int, Int)

--type Stage = Map Loc Object
{-
type Inventory = Map Item Int

data Action = Move | Fire | Inv
  deriving (Eq, Show)

data Item = Bullet | Life | Speed | Teleport
  deriving (Eq, Show, Ord)

--data Object = Player | Enemy | Obstacle | Goal | Projectile Direction | Collectable Item | Vacant | Boom
--  deriving (Eq)


  
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
-}
--data Direction = North | South | East | West | Stay
  --deriving (Eq, Show)

chase :: Loc -> Loc -> Stage -> Direction
chase enemy player s = if canMove (nudge enemy (direct enemy player)) s then direct enemy player else Stay 

canMove :: Loc -> Stage -> Bool
canMove l s = case Map.lookup l s of
  Just Player -> True
  Just (Projectile _) -> True
  Just Vacant -> True
  Just Boom -> True
  otherwise -> False

direct :: Loc -> Loc -> Direction
direct (a,b) (c,d) = if abs (a-c) > abs (b-d)
                          then if (a-c) > 0
                               then West
                               else East
                          else if (b-d) > 0
                               then North
                               else South
{-
hValue :: Loc -> Loc -> Int
hValue (a,b) (c,d) = (abs (a-c)) + (abs (b-d))

aStar :: Loc -> Loc -> Stage -> Direction
aStar enemy player s | enemy == player = Stay
                     | otherwise = helper False (insert enemy (enemy, 0, 0, 0) Map.empty) Map.empty enemy player s

helper :: Bool -> Deets -> Closed -> Loc -> Loc -> Stage -> Direction
helper b open closed start end s = case Map.keys open of
  (x:xs) -> (insert (open ! x) True closed) (delete x open)
  [] -> undefined

processChild :: Loc -> Deets -> Closed -> Cell -> Loc -> Stage -> (Deets, Closed, Bool, Cell)
processChild child open closed (pl, pg, ph, pf) end s = case canMove child s of
  True -> if child == end
            then (open, closed, True, (pl, (-1), (-1), (-1)))
          else if (Map.notMember child closed) == True
            then (o', c', False, (pl, gnew, hnew, fnew)) 
          else (open, closed, False, (((-1),(-1)), 9999, 9999, 99999)) where
            gnew = pg + 1
            hnew = hValue child end
            fnew = hnew + gnew
            o' = case Map.lookup child open of
              Just (_,_,_,f) -> if f < fnew then open else insert child (pl, gnew, hnew, fnew) open
              Nothing -> insert child (pl, gnew, hnew, fnew) open
            c' = insert child True closed
   
  False -> (open, closed, False, (((-1),(-1)), 9999, 9999, 99999))

firstStep :: Cell -> Deets -> Direction
firstStep (pl, _, _, _) d = Map.lookup
-}
nudge :: Loc -> Direction -> Loc
nudge (x,y) d = case d of
  Stay -> (x,y)
  North -> (x, y-1)
  South -> (x, y+1)
  East -> (x+1,y)
  West -> (x-1,y)



  
