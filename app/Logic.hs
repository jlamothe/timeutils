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

import Brick.BChan (writeBChan)
import Brick.Main (continue, halt)
import Brick.Types (BrickEvent (..), EventM, Next)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (NominalDiffTime, getCurrentTime)
import Lens.Micro (over, sets)
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
    VtyEvent (EvKey (KChar '\t') [])     -> continue $ changeMode s'
    AppEvent ()                          -> ping s' >> continue s'
    VtyEvent ev'                         -> continue $
      case progMode s of
        StopwatchMode -> stopwatchEvent s' ev'
        CountdownMode -> countdownEvent s' ev'
    _ -> continue s'

changeMode :: ProgState -> ProgState
changeMode s = s
  { progMode = case progMode s of
    StopwatchMode -> CountdownMode
    CountdownMode -> StopwatchMode
  }

ping :: ProgState -> EventM () ()
ping s = void $ liftIO $ forkIO $ do
  threadDelay 100000
  writeBChan (channel s) ()

stopwatchEvent
  :: ProgState
  -> Event
  -> ProgState
stopwatchEvent s (EvKey (KChar ' ') []) = let
  t   = currentTime s
  sw  = stopwatch s
  sw' = if stopwatchIsRunning sw
    then stopStopwatchAt t sw
    else startStopwatchAt t sw
  in s { stopwatch = sw' }
stopwatchEvent s (EvKey (KChar 'l') []) = let
  t   = currentTime s
  sw  = stopwatch s
  sw' = newLapAt t sw
  in s { stopwatch = sw' }
stopwatchEvent s (EvKey (KChar 'r') []) =
  s { stopwatch = newStopwatch }
stopwatchEvent s _ = s

countdownEvent
  :: ProgState
  -> Event
  -> ProgState
countdownEvent s ev = case ev of
  EvKey (KChar 'n') [] -> newCd s
  EvKey KUp []         -> prevCd s
  EvKey KDown []       -> nextCd s
  EvKey (KChar 'd') [] -> adjustCd day s
  EvKey (KChar 'D') [] -> adjustCd (-day) s
  EvKey (KChar 'h') [] -> adjustCd hour s
  EvKey (KChar 'H') [] -> adjustCd (-hour) s
  EvKey (KChar 'm') [] -> adjustCd minute s
  EvKey (KChar 'M') [] -> adjustCd (-minute) s
  EvKey (KChar 's') [] -> adjustCd second s
  EvKey (KChar 'S') [] -> adjustCd (-second) s
  _                    -> s

newCd :: ProgState -> ProgState
newCd s = s
  { countdowns   = newCountdown 0 : cds
  , countdownSel = Just 0
  } where cds = countdowns s

prevCd :: ProgState -> ProgState
prevCd s = s
  { countdownSel = case countdownSel s of
    Just 0  -> Nothing
    Just n  -> Just $ pred n
    Nothing -> if null $ countdowns s
      then Nothing
      else Just $ pred $ length $ countdowns s
  }

nextCd :: ProgState -> ProgState
nextCd s = s
  { countdownSel = case countdownSel s of
    Nothing -> Just 0
    Just n  -> let n' = succ n
      in if n' >= length (countdowns s)
        then Nothing
        else Just n'
  }

adjustCd :: NominalDiffTime -> ProgState -> ProgState
adjustCd dt = over (sets updateCd . countdownLengthL) $
  max 0 . (+dt)

updateCd :: (Countdown -> Countdown) -> ProgState -> ProgState
updateCd f s = s
  { countdowns = map
    (\(i, cd) -> if Just i == countdownSel s
      then f cd
      else cd)
    (zip [0..] $ countdowns s)
  }

day :: NominalDiffTime
day = hour * 24

hour :: NominalDiffTime
hour = minute * 60

minute :: NominalDiffTime
minute = second * 60

second :: NominalDiffTime
second = 1

-- jl
