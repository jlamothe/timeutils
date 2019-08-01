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

module Types
  ( ProgState (..)
  , ProgMode (..)
  , newProgState
  ) where

import Brick.BChan (BChan, newBChan)
import Data.Time (UTCTime, getCurrentTime)

import Data.Time.Utils

data ProgState = ProgState
  { currentTime  :: UTCTime
  , stopwatch    :: Stopwatch
  , countdowns   :: [Countdown]
  , progMode     :: ProgMode
  , countdownSel :: Maybe Int
  , channel      :: BChan ()
  }

data ProgMode = StopwatchMode | CountdownMode
  deriving (Eq, Show)

newProgState :: IO ProgState
newProgState = do
  t    <- getCurrentTime
  chan <- newBChan 10
  return ProgState
    { currentTime  = t
    , stopwatch    = newStopwatch
    , countdowns   = []
    , progMode     = StopwatchMode
    , countdownSel = Nothing
    , channel      = chan
    }

-- jl
