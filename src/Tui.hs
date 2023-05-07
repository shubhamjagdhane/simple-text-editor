{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Tui where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Graphics.Vty as V

data CustomEvent = Counter deriving Show

data St = 
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent) 
       , _stCounter :: Int
       }
    deriving Show


makeLenses ''St

theApp :: App St CustomEvent ()
theApp = 
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }


drawUI :: St -> [Widget ()]
drawUI st = [a]
    where
        a = (str $ "Last Event: " <> (show $ st ^. stLastBrickEvent))
            <=>
            (str $ "Counter value is:" <> (show $ st ^. stCounter))

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e = 
    case e of
      VtyEvent (V.EvKey V.KEsc []) -> halt
      VtyEvent _ -> do
          stCounter %= (+1)
          stLastBrickEvent .= Just e
      _ -> return ()

initialState :: St
initialState = 
    St { _stLastBrickEvent = Nothing
       , _stCounter = 0
       }

tui :: IO ()
tui = do
    endState <- defaultMain theApp initialState
    print endState

