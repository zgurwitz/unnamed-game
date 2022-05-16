{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import System.IO.Unsafe
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
handleEvent g ev = case (g ^. action, g ^. done, ev) of
  (_, _,AppEvent Tick) -> continue $ step g
  (_, _,(VtyEvent (V.EvKey (V.KChar 'q') []))) -> halt g
  (_, Ongoing, (VtyEvent (V.EvKey (V.KChar 'c') []))) -> continue $ cycleAction g
  (Move, Ongoing, (VtyEvent (V.EvKey V.KUp    []))) -> continue $ checkAliveAfterAction Move North g
  (Move, Ongoing, (VtyEvent (V.EvKey V.KDown    []))) -> continue $ checkAliveAfterAction Move South g
  (Move, Ongoing, (VtyEvent (V.EvKey V.KRight    []))) -> continue $ checkAliveAfterAction Move East g
  (Move, Ongoing, (VtyEvent (V.EvKey V.KLeft    []))) -> continue $ checkAliveAfterAction Move West g
  (Move, Ongoing, (VtyEvent (V.EvKey (V.KChar 'x') []))) -> continue $ checkAliveAfterAction Move Stay g
  (Fire, Ongoing, (VtyEvent (V.EvKey V.KUp    []))) -> continue $ checkAliveAfterAction Fire North g
  (Fire, Ongoing, (VtyEvent (V.EvKey V.KDown    []))) -> continue $ checkAliveAfterAction Fire South g
  (Fire, Ongoing, (VtyEvent (V.EvKey V.KRight    []))) -> continue $ checkAliveAfterAction Fire East g
  (Fire, Ongoing, (VtyEvent (V.EvKey V.KLeft    []))) -> continue $ checkAliveAfterAction Fire West g
  (_, Won, (VtyEvent (V.EvKey V.KEnter         []))) -> continue $ unsafePerformIO (newLevel g Won) 
  (_, Lost, (VtyEvent (V.EvKey V.KEnter         []))) -> continue $ unsafePerformIO (newLevel g Lost) 
  otherwise -> continue g

drawUI :: Game -> [Widget Name]
drawUI g = [vBox [ withBorderStyle BS.unicodeBold
                   $ B.borderWithLabel (str "Game")
                   $ drawGrid g
                 , drawAction g
                 , drawInv g
                 , drawScore g
                 , drawAdvance g
                 , drawGameOver g 
                 , drawRules]]

drawGameOver :: Game -> Widget Name
drawGameOver g
  | g ^. done == Lost = str "You Lose!"
  | g ^. done == Won = str "You Win!"
  | otherwise = emptyWidget

--currently only draws bullets
drawInv :: Game -> Widget Name
drawInv g = str $ showItem (g ^. inventory) Bullet 

drawAction :: Game -> Widget Name
drawAction g = str $ show $ g ^. action

drawScore :: Game -> Widget Name
drawScore g = str $ "Score: " ++ ( show $ g ^. score)

drawAdvance :: Game -> Widget Name
drawAdvance g 
  | g ^. done == Won = str "Well done! Press Enter to go to next stage."
  | g ^. done == Lost = str "Good try! Press Enter to restart."
  | otherwise = str ""

drawRules :: Widget Name
drawRules = str $ "-----------------------------------------------\nCONTROLS:\nYou are the (@)\nUse the arrow keys to move/fire, or 'x' to pass your move. \nUse 'c' to change from moving to firing. \nDon't touch the enemies (e) and try to get to the goal (f)! \n(0)s are Bullets that you can pick up to shoot at enemies.\n(x)s are abstacles. You can't move onto them, but neither can your enemies!\nPress (q) to exit at any time"

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
