{-

timeutils

Copyright (C) 2019 Jonathan Lamothe
<jlamothe1980@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-}

module Logic (handleEvent) where

import Brick.Main (continue, halt)
import Brick.Types (BrickEvent (..), EventM, Next)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Graphics.Vty.Input.Events
  ( Event (..)
  , Key (..)
  , Modifier (..)
  )

import Data.Time.Utils
import Types

handleEvent
  :: ProgState
  -> BrickEvent () ()
  -> EventM () (Next ProgState)
handleEvent s ev = do
  t <- liftIO getCurrentTime
  let s' = s { currentTime = t }
  case ev of
    VtyEvent (EvKey (KChar 'q') [])      -> halt s'
    VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s'
    VtyEvent (EvKey KEsc [])             -> halt s'
    VtyEvent (EvKey (KChar '\t') [])     -> changeMode s'
    _                                    -> case progMode s of
      StopwatchMode -> stopwatchEvent s' ev
      CountdownMode -> countdownEvent s' ev

changeMode :: ProgState -> EventM () (Next ProgState)
changeMode s = continue s
  { progMode = case progMode s of
    StopwatchMode -> CountdownMode
    CountdownMode -> StopwatchMode
  }

stopwatchEvent
  :: ProgState
  -> BrickEvent () ()
  -> EventM () (Next ProgState)
stopwatchEvent s (VtyEvent (EvKey (KChar ' ') [])) = do
  let sw = stopwatch s
  sw' <- if stopwatchIsRunning sw
    then liftIO $ stopStopwatch sw
    else liftIO $ startStopwatch sw
  continue s { stopwatch = sw' }
stopwatchEvent s _ = continue s

countdownEvent
  :: ProgState
  -> BrickEvent () ()
  -> EventM () (Next ProgState)
countdownEvent s _ = continue s

-- jl
