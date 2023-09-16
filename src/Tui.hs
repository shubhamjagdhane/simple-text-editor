{-# LANGUAGE OverloadedStrings #-}

module Tui(tui) where

import System.Directory
import Brick.Main
import  Brick.Types
import Brick.AttrMap
    ( attrMap
    )
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events


tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState = TuiState 
    { tuiStatePaths :: [FilePath]
    } deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp = 
    App 
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

buildInitialState :: IO TuiState
buildInitialState = do
    here <- getCurrentDirectory
    contents <- getDirectoryContents here
    pure TuiState { tuiStatePaths = contents }

--drawTui :: TuiState -> [Widget ()]
--drawTui _ts = str "Hello World"

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [ vBox $ map drawPath $ tuiStatePaths ts ]

drawPath :: FilePath -> Widget n
drawPath = str 

handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
    {-
    case e of 
      VtyEvent vtye -> 
          case vtye of
            EvKey (KChar 'q') [] -> halt s
            _ -> continue s 
      _ -> continue s

-}
