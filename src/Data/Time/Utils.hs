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

module Data.Time.Utils (
  -- * Types
  Timer (..),
  TimeParts (..),
  Countdown (..),
  Stopwatch (..),
  -- * Constructors
  newTimer,
  newTimeParts,
  newCountdown,
  newStopwatch,
  -- * Timer Functions
  startTimer,
  stopTimer,
  timeElapsed,
  -- * Countdown Functions
  startCountdown,
  stopCountdown,
  timeRemaining,
  countdownIsCompleted,
  -- * Stopwatch Functions
  currentLap,
  allLaps,
  totalStopwatchLength,
  -- * Pure Functions
  decomposeTime,
  composeTime,
  -- ** Timer Functions
  timerIsRunning,
  timerIsStarted,
  startTimerUsing,
  stopTimerUsing,
  timeElapsedUsing,
  -- ** Countdown Functions
  startCountdownUsing,
  stopCountdownUsing,
  timeRemainingUsing,
  countdownIsCompletedUsing,
  countdownIsRunning,
  countdownIsStarted,
  -- ** Stopwatch Functions
  currentLapUsing,
  allLapsUsing,
  totalStopwatchLengthUsing
) where

import Data.Maybe (isJust, isNothing)
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
    -- ^ The time the 'Timer' was last started ('Nothing' if not
    -- currently running)
  } deriving (Eq, Show)

-- | Represents a 'NominalDiffTime' in a more human readable format
data TimeParts = TimeParts
  { tpDays    :: Int
    -- ^ The number of days
  , tpHours   :: Int
    -- ^ The number of hours
  , tpMinutes :: Int
    -- ^ The number of minutes
  , tpSeconds :: Int
    -- ^ The number of seconds
  , tpMillis  :: Int
    -- ^ The number if milliseconds
  } deriving (Eq, Show)

-- | Represents a timed countdown
data Countdown = Countdown
  { countdownLength :: NominalDiffTime
  -- ^ The length of time
  , countdownTimer  :: Timer
  -- ^ The timer which runs the 'Countdown'
  } deriving (Eq, Show)

-- | Tracks the time of multiple laps
data Stopwatch = Stopwatch
  { stopwatchTimer :: Timer
  -- ^ The 'Timer' for the current lap
  , stopwatchLaps  :: [NominalDiffTime]
  -- ^ The times of previous laps (most recent first)
  } deriving (Eq, Show)

-- | New instance of a 'Timer'
newTimer :: Timer
newTimer = Timer 0 Nothing

-- | New instance of a 'TimeParts' value
newTimeParts :: TimeParts
newTimeParts = TimeParts 0 0 0 0 0

-- | New instance of a 'Countdown'
newCountdown
  :: NominalDiffTime
  -- ^ The time length
  -> Countdown
newCountdown l = Countdown l newTimer

-- | New instance of a stopwatch
newStopwatch :: Stopwatch
newStopwatch = Stopwatch
  { stopwatchTimer = newTimer
  , stopwatchLaps  = []
  }

-- | Starts a 'Timer'
startTimer
  :: Timer
  -- ^ The 'Timer' being started
  -> IO Timer
  -- ^ The modified 'Timer'
startTimer timer = startTimerUsing
  <$> getCurrentTime
  <*> return timer

-- | Stops a 'Timer'
stopTimer
  :: Timer
  -- ^ The 'Timer' being stopped
  -> IO Timer
  -- ^ The modified 'Timer'
stopTimer timer = stopTimerUsing
  <$> getCurrentTime
  <*> return timer

-- | Calculates the amount of time elapsed on a 'Timer'
timeElapsed
  :: Timer
  -- ^ The 'Timer' being checked
  -> IO NominalDiffTime
  -- ^ The amount of time elapsed
timeElapsed timer = timeElapsedUsing
  <$> getCurrentTime
  <*> return timer

-- | Starts a 'Countdown'
startCountdown
  :: Countdown
  -- ^ The 'Countdown' being started
  -> IO Countdown
  -- ^ Returns the modified 'Countdown'
startCountdown countdown = startCountdownUsing
  <$> getCurrentTime
  <*> return countdown

-- | Stops a 'Countdown'
stopCountdown
  :: Countdown
  -- ^ The 'Countdown' being stopped
  -> IO Countdown
  -- ^ Returns the modified 'Countdown'
stopCountdown countdown = stopCountdownUsing
  <$> getCurrentTime
  <*> return countdown

-- | Calculates the amount of time remaining in a 'Countdown'
timeRemaining
  :: Countdown
  -- ^ The 'Countdown' being checked
  -> IO NominalDiffTime
  -- ^ The amount of time remaining
timeRemaining countdown = timeRemainingUsing
  <$> getCurrentTime
  <*> return countdown

-- | Determines whether or not a 'Countdown' has completed.
countdownIsCompleted
  :: Countdown
  -- ^ The 'Countdown' being checked
  -> IO Bool
  -- ^ Returns 'True' if the 'Countdown' has completed, 'False'
  -- otherwise
countdownIsCompleted countdown = countdownIsCompletedUsing
  <$> getCurrentTime
  <*> return countdown

-- | Returns the time of the current lap from a 'Stopwatch'
currentLap
  :: Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> IO NominalDiffTime
  -- ^ Returns the amount of time elapsed in the current lap
currentLap stopwatch = currentLapUsing
  <$> getCurrentTime
  <*> return stopwatch

-- | Returns the lap times for a 'Stopwatch'
allLaps
  :: Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> IO [NominalDiffTime]
  -- ^ Returns the lap times (most recent first)
allLaps stopwatch = allLapsUsing
  <$> getCurrentTime
  <*> return stopwatch

-- | Calculates the total run time of a 'Stopwatch'
totalStopwatchLength
  :: Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> IO NominalDiffTime
  -- ^ Returns the total run time
totalStopwatchLength stopwatch = totalStopwatchLengthUsing
  <$> getCurrentTime
  <*> return stopwatch

-- | Converts a 'NominalDiffTime' to a 'TimeParts' value
decomposeTime :: NominalDiffTime -> TimeParts
decomposeTime t = TimeParts
  { tpDays    = days
  , tpHours   = hours
  , tpMinutes = minutes
  , tpSeconds = seconds
  , tpMillis  = millis
  }
  where
    days    = h `quot` 24
    hours   = h - days * 24
    minutes = m - h * 60
    seconds = s - m * 60
    millis  = ms - s * 1000
    h       = m `quot` 60
    m       = s `quot` 60
    s       = ms `quot` 1000
    ms      = floor $ t * 1000

-- | Converts a 'TimeParts' value to a 'NominalDiffTime'
composeTime :: TimeParts -> NominalDiffTime
composeTime tp = fromInteger millis / 1000
  where
    millis  = seconds * 1000 + toInteger (tpMillis tp)
    seconds = minutes * 60 + toInteger (tpSeconds tp)
    minutes = hours * 60 + toInteger (tpMinutes tp)
    hours   = toInteger (tpDays tp) * 24 + toInteger (tpHours tp)

-- | Determines whether or not a 'Timer' is running
timerIsRunning
  :: Timer
  -- ^ The 'Timer' being checked
  -> Bool
timerIsRunning = isJust . timerStartTime

-- | Determines whether or not a 'Timer' has been started (even if it
-- is currently stopped)
timerIsStarted
  :: Timer
  -- ^ The 'Timer' being checked
  -> Bool
  -- ^ 'True' if the timer has been started, 'False' otherwise
timerIsStarted timer = timerIsRunning timer ||
  timerOffset timer /= 0

-- | Starts a 'Timer' from a given time
startTimerUsing
  :: UTCTime
  -- ^ The current time
  -> Timer
  -- ^ The 'Timer' being started
  -> Timer
  -- ^ The modified 'Timer'
startTimerUsing t timer
  | isNothing (timerStartTime timer) =
    timer { timerStartTime = Just t }
  | otherwise = timer

-- | Stops a 'Timer' from a given time
stopTimerUsing
  :: UTCTime
  -- ^ The current time
  -> Timer
  -- ^ The 'Timer' being stopped
  -> Timer
  -- ^ The modified 'Timer'
stopTimerUsing t timer = newTimer { timerOffset = offset }
  where offset = timeElapsedUsing t timer

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

-- | Starts a 'Countdown' using a given time
startCountdownUsing
  :: UTCTime
  -- ^ The current time
  -> Countdown
  -- ^ The 'Countdown' being started
  -> Countdown
  -- ^ The modified 'Countdown'
startCountdownUsing t countdown =
  countdown { countdownTimer = timer' }
  where
    timer' = startTimerUsing t timer
    timer  = countdownTimer countdown

-- | Stops a 'Countdown' using a given time
stopCountdownUsing
  :: UTCTime
  -- ^ The current time
  -> Countdown
  -- ^ The 'Countdown' being stopped
  -> Countdown
  -- ^ The modified 'Countdown'
stopCountdownUsing t countdown =
  countdown { countdownTimer = timer' }
  where
    timer' = stopTimerUsing t timer
    timer  = countdownTimer countdown

-- | Calculates the amount of time remaining in a 'Countdown' at a
-- given time
timeRemainingUsing
  :: UTCTime
  -- ^ The current time
  -> Countdown
  -- ^ The 'Countdown' being checked
  -> NominalDiffTime
  -- ^ The amount of time remaining
timeRemainingUsing t countdown = len - timeElapsedUsing t timer
  where
    len   = countdownLength countdown
    timer = countdownTimer countdown

-- | Determines if a 'Countdown' is completed at a given time
countdownIsCompletedUsing
  :: UTCTime
  -- ^ The current time
  -> Countdown
  -- ^ The 'Countdown' being checked
  -> Bool
  -- ^ Returns 'True' if the 'Countdown' has completed, 'False'
  -- otherwise.
countdownIsCompletedUsing t countdown =
  timeRemainingUsing t countdown <= 0

-- | Determines whether or not a 'Countdown' is running
countdownIsRunning
  :: Countdown
  -- ^ The 'Countdown' being checked
  -> Bool
  -- ^ 'True' if running, 'False' otherwise
countdownIsRunning countdown = timerIsRunning timer
  where timer = countdownTimer countdown

-- | Determines whether or not a 'Countdown' has been started (even if
-- subsequently stopped)
countdownIsStarted
  :: Countdown
  -- ^ The 'Countdown' being checked
  -> Bool
  -- ^ 'True' if it has been started, 'False' otherwise
countdownIsStarted countdown = timerIsStarted timer
  where timer = countdownTimer countdown

-- | Determines the length of the current lap given a time
currentLapUsing
  :: UTCTime
  -- ^ The current time
  -> Stopwatch
  -- ^ The 'Stopwatch' being checkwd
  -> NominalDiffTime
  -- ^ The elapsed time for the current lap
currentLapUsing t stopwatch = timeElapsedUsing t timer
  where timer = stopwatchTimer stopwatch

-- | Returns the lap times from a 'Stopwatch' given a time
allLapsUsing
  :: UTCTime
  -- ^ The current time
  -> Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> [NominalDiffTime]
  -- ^ The lap times (most recent first)
allLapsUsing t stopwatch = currentLapUsing t stopwatch :
  stopwatchLaps stopwatch

-- | Calculates the total runtime of a 'Stopwatch' given a time
totalStopwatchLengthUsing
  :: UTCTime
  -- ^ The current time
  -> Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> NominalDiffTime
  -- ^ The total run time
totalStopwatchLengthUsing t stopwatch =
  sum $ allLapsUsing t stopwatch

-- jl
