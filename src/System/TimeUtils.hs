{- |

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

module System.TimeUtils (
  -- * Types
  Timer (..),
  -- * Constructors
  newTimer,
  -- * Timer Functions
  timeElapsed,
  -- * Pure Functions
  timeElapsedUsing
) where

import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime
  , diffUTCTime
  , getCurrentTime
  )

-- | A type that keeps track of the passage of time
data Timer = Timer
  { timerOffset    :: NominalDiffTime
  -- ^ The amount of time previously logged
  , timerStartTime :: Maybe UTCTime
  -- ^ The the time the 'Timer' was last started ('Nothing' if not
  -- currently running)
  } deriving (Eq, Show)

-- | New instance of a 'Timer'
newTimer :: Timer
newTimer = Timer 0 Nothing

-- | Calculates the amount of time elapsed on a 'Timer'
timeElapsed :: Timer -> IO NominalDiffTime
timeElapsed timer = timeElapsedUsing
  <$> getCurrentTime
  <*> return timer

-- | Calculates the amount of time elapsed on a 'Timer' from a given
-- time
timeElapsedUsing
  :: UTCTime
  -- ^ The current time
  -> Timer
  -- ^ The 'Timer' being checked
  -> NominalDiffTime
  -- ^ The amount of time elapsed
timeElapsedUsing t timer = case timerStartTime timer of
  Nothing -> timerOffset timer
  Just st -> timerOffset timer + diffUTCTime t st

-- jl
