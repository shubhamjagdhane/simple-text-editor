{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}


module Editor where

import System.Directory
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events

tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print initialState 


data TuiState = TuiState 
    { tuiStatePaths :: [FilePath]
    } deriving (Show, Eq)

buildInitialState :: IO TuiState
buildInitialState = do
    here <- getCurrentDirectory
    contents <- getDirectoryContents here
    pure TuiState { tuiStatePaths = contents}

tuiApp :: App TuiState () ()
tuiApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

-- vBox :: [Widget n] -> Widget n
drawUI :: TuiState -> [Widget ()]
drawUI ts = [ vBox $ map drawPath $ tuiStatePaths ts]

drawPath :: FilePath -> Widget n
drawPath = str

handleTuiEvent :: BrickEvent () () -> EventM () TuiState ()
handleTuiEvent e =
    case e of
      VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
      _ -> return () -- update this with TuiState

