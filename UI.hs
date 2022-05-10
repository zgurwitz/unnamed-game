{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import Game
import Datas
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg, bg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), at)
import qualified Graphics.Vty as V

import Data.Maybe 
import Data.List  (intersperse)

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g ev = case (g ^. action, ev) of
  (_, AppEvent Tick) -> continue $ step g
  (_, (VtyEvent (V.EvKey (V.KChar 'q') []))) -> halt g
  (_, (VtyEvent (V.EvKey (V.KChar 'c') []))) -> continue $ cycleAction g
  (Move, (VtyEvent (V.EvKey V.KUp    []))) -> continue $ playerAction Move North g
  (Move, (VtyEvent (V.EvKey V.KDown    []))) -> continue $ playerAction Move South g
  (Move, (VtyEvent (V.EvKey V.KRight    []))) -> continue $ playerAction Move East g
  (Move, (VtyEvent (V.EvKey V.KLeft    []))) -> continue $ playerAction Move West g
  (Fire, (VtyEvent (V.EvKey V.KUp    []))) -> continue $ playerAction Fire North g
  (Fire, (VtyEvent (V.EvKey V.KDown    []))) -> continue $ playerAction Fire South g
  (Fire, (VtyEvent (V.EvKey V.KRight    []))) -> continue $ playerAction Fire East g
  (Fire, (VtyEvent (V.EvKey V.KLeft    []))) -> continue $ playerAction Move West g
  otherwise -> continue g

drawUI :: Game -> [Widget Name]
drawUI g = [vBox [ withBorderStyle BS.unicodeBold
                   $ B.borderWithLabel (str "Game")
                   $ drawGrid g
                 , drawGameOver g ]]

drawGameOver :: Game -> Widget Name
drawGameOver g
  | g ^. done == Lost = str "You Lose!"
  | g ^. done == Won = str "You Win!"
  | otherwise = emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = vBox rows where
  rows = [hBox $ cells y | y <- [0..(snd (g ^. stageDim) - 1)]]
  cells y = [drawCell x y | x <- [0..(fst (g ^. stageDim) - 1)]]
  drawCell x y = case g ^. stage . at (x,y) of
    Nothing -> str " "
    Just p -> str (show p)

theMap :: AttrMap
theMap = attrMap V.defAttr [(cursorAttr, bg V.red)]

cursorAttr :: AttrName
cursorAttr = "cursorAttr"
