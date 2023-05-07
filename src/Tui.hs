module Tui where

import Brick

tui :: IO()
tui = simpleMain ui

ui :: Widget ()
ui = str "Hello World"
