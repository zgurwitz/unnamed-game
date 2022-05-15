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
import Search

chase :: Loc -> Loc -> Stage -> Direction
chase enemy player s = if canMove (nudge enemy (direct enemy player)) s then direct enemy player else Stay 

chase' :: Loc -> Loc -> Stage -> Direction
chase' enemy player s = case gBFS s enemy player IsEnemy of
  Nothing -> Stay
  Just d -> d

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
