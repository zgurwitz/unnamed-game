module Search where

import Datas
import Data.Map (Map, notMember, member, singleton, (!), insert )

qPut :: (Ord b) => [(a,b)] -> (a,b) -> [(a,b)]
qPut [] (a,b) = [(a,b)]
qPut ((ax,bx):xs) (a,b) = if b > bx then (ax,bx):qPut xs (a,b) else (a,b):(ax,bx):xs

qPop :: (Ord b) => [(a,b)] -> Maybe (a,[(a,b)])
qPop ((a,b):xs) = Just (a,xs)
qPop [] = Nothing

inList :: (Eq a) => [a] -> a -> Bool
inList [] _ = False
inList (x:xs) a = if a == x then True else inList xs a 

dist :: Loc -> Loc -> Int
dist (a,b) (c,d) = abs(a-c) + abs (b-d)

neighbors :: Stage -> Loc -> [Loc]
neighbors s l = foldr f [] ls where 
  f lo ns = if member lo s then lo:ns else ns
  ls = [nudge l North, nudge l South, nudge l East, nudge l West] 

gBFS :: Stage -> Loc -> Loc -> MobileEntity -> Maybe Direction
gBFS s start goal me = helper s goal [(start, 0)] (singleton start Stay) me 

helper :: Stage -> Loc -> [(Loc, Int)] -> Map Loc Direction -> MobileEntity -> Maybe Direction
helper _ _ [] _ _ = Nothing
helper s g ((l,w):qs) cameFrom me = if l == g then Just (findDir cameFrom g g) else helper s g newQ newCameFrom me where
  (newQ, newCameFrom) = helpNeighbors (neighbors s l) qs cameFrom
  helpNeighbors (n:ns) qs came = if member n came || notMember n s || not (canMoveHere s n me) then helpNeighbors ns qs came
    else helpNeighbors ns (qPut qs (n,w+1)) (insert n (dir n l) came)   
  helpNeighbors [] qs came = (qs, came)

findDir :: Map Loc Direction -> Loc -> Loc -> Direction
findDir came n prev = if came ! n == Stay then came ! prev else findDir came (nudge n (came ! n)) n  

dir :: Loc -> Loc -> Direction
dir (a,b) (c,d) = case (a-c, b-d) of
  (0,0) -> Stay
  (1,0) -> West
  (0,1) -> North
  (-1,0) -> East
  (0,-1) -> South
  otherwise -> undefined
