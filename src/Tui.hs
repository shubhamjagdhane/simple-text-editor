{-# LANGUAGE OverloadedStrings #-}

module Tui(tui) where

import System.Directory
import System.Exit
import qualified Brick as B
import Brick.Main
import  Brick.Types
import  Brick.Util
import Brick.AttrMap
    ( attrMap
    )
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as NE


tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

newtype TuiState = TuiState 
    { tuiStatePaths :: NonEmptyCursor FilePath
    } deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp = 
    App 
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure ()
        , appAttrMap = const $ attrMap V.defAttr [(B.attrName "selected", fg red)]
        }

buildInitialState :: IO TuiState
buildInitialState = do
    here <- getCurrentDirectory
    contents <- getDirectoryContents here
    case NE.nonEmpty contents of
      Nothing -> die "There are no contents."
      Just ne -> pure TuiState { tuiStatePaths = makeNonEmptyCursor ne }

--drawTui :: TuiState -> [Widget ()]
--drawTui _ts = str "Hello World"

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = 
    let nec = tuiStatePaths ts
     in [ vBox $
         concat
            [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
            , [drawPath True $ nonEmptyCursorCurrent nec]
            , map (drawPath False) $ nonEmptyCursorNext nec
            ]
        ]

drawPath :: Bool -> FilePath -> Widget n
drawPath False = str 
drawPath True = B.withAttr (B.attrName "selected") . str
{--
    drawPath b = 
        (if b 
            then B.withAttr (B.attrName "selected")
            else id) . 
        str
--}

handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleTuiEvent (VtyEvent (V.EvKey KDown [])) = do
    st <- get
    let nec = tuiStatePaths st
    case nonEmptyCursorSelectNext nec of 
        Nothing -> continueWithoutRedraw 
        Just nec' -> put $ TuiState { tuiStatePaths = nec' }
handleTuiEvent (VtyEvent (V.EvKey KUp [])) = do
    st <- get
    let nec = tuiStatePaths st
    case nonEmptyCursorSelectPrev nec of 
        Nothing -> continueWithoutRedraw 
        Just nec' -> put $ TuiState { tuiStatePaths = nec' }
handleTuiEvent _= halt
