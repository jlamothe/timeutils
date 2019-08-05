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
  -- * Lenses
  -- ** Timer Lenses
  timerOffsetL,
  timerStartTimeL,
  -- ** TimeParts Lenses
  tpDaysL,
  tpHoursL,
  tpMinutesL,
  tpSecondsL,
  tpMillisL,
  -- ** Countdown Lenses
  countdownLengthL,
  countdownTimerL,
  -- ** Stopwatch Lenses
  stopwatchTimerL,
  stopwatchLapsL,
  -- * Constructors
  newTimer,
  newTimeParts,
  newCountdown,
  newStopwatch,
  -- * Impure Functions
  -- ** Timer Functions
  startTimer,
  stopTimer,
  timeElapsed,
  -- ** Countdown Functions
  startCountdown,
  stopCountdown,
  timeRemaining,
  countdownIsCompleted,
  -- ** Stopwatch Functions
  startStopwatch,
  stopStopwatch,
  newLap,
  currentLap,
  allLaps,
  totalStopwatchTime,
  -- * Pure Functions
  decomposeNDT,
  composeNDT,
  humanNDT,
  -- ** Timer Functions
  timerIsRunning,
  timerIsStarted,
  startTimerAt,
  stopTimerAt,
  timeElapsedAt,
  -- ** Countdown Functions
  countdownIsRunning,
  countdownIsStarted,
  startCountdownAt,
  stopCountdownAt,
  timeRemainingAt,
  countdownIsCompletedAt,
  -- ** Stopwatch Functions
  stopwatchIsRunning,
  stopwatchIsStarted,
  startStopwatchAt,
  stopStopwatchAt,
  newLapAt,
  currentLapAt,
  allLapsAt,
  totalStopwatchTimeAt
) where

import Data.Maybe (isJust, isNothing)
import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime
  , diffUTCTime
  , getCurrentTime
  )
import Lens.Micro (Lens', lens)

-- | A type that keeps track of the passage of time
data Timer = Timer
  { timerOffset    :: NominalDiffTime
    -- ^ The amount of time previously logged
  , timerStartTime :: Maybe UTCTime
    -- ^ The time the 'Timer' was last started ('Nothing' if not
    -- currently running)
  } deriving (Eq, Show)

-- | Lens for the 'timerOffset' attribute
timerOffsetL :: Lens' Timer NominalDiffTime
timerOffsetL = lens timerOffset $
  \t o -> t { timerOffset = o }

-- | Lens for the 'timerStartTime' attribute
timerStartTimeL :: Lens' Timer (Maybe UTCTime)
timerStartTimeL = lens timerStartTime $
  \t st -> t { timerStartTime = st }

-- | Represents a 'NominalDiffTime' broken down into days, hours,
-- minutes, seconds and milliseconds
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
    -- ^ The number of milliseconds
  } deriving (Eq, Show)

-- | Lens for the 'tpDays' attribute
tpDaysL :: Lens' TimeParts Int
tpDaysL = lens tpDays $
  \tp d -> tp { tpDays = d }

-- | Lens for the 'tpHours' attribute
tpHoursL :: Lens' TimeParts Int
tpHoursL = lens tpHours $
  \tp h -> tp { tpHours = h }

-- | Lens for the 'tpMinutes' attribute
tpMinutesL :: Lens' TimeParts Int
tpMinutesL = lens tpMinutes $
  \tp m -> tp { tpMinutes = m }

-- | Lens for the 'tpSeconds' attribute
tpSecondsL :: Lens' TimeParts Int
tpSecondsL = lens tpSeconds $
  \tp s -> tp { tpSeconds = s }

-- | Lens for the 'tpMillis' attribute
tpMillisL :: Lens' TimeParts Int
tpMillisL = lens tpMillis $
  \tp ms -> tp { tpMillis = ms }

-- | Represents a timed countdown
data Countdown = Countdown
  { countdownLength :: NominalDiffTime
  -- ^ The length of time
  , countdownTimer  :: Timer
  -- ^ The timer which runs the 'Countdown'
  } deriving (Eq, Show)

-- | Lens for the 'countdownLength' attribute
countdownLengthL :: Lens' Countdown NominalDiffTime
countdownLengthL = lens countdownLength $
  \cd l -> cd { countdownLength = l }

-- | Lens for the 'countdownTimer' attribute
countdownTimerL :: Lens' Countdown Timer
countdownTimerL = lens countdownTimer $
  \cd t -> cd { countdownTimer = t }

-- | Tracks the time of multiple laps
data Stopwatch = Stopwatch
  { stopwatchTimer :: Timer
  -- ^ The 'Timer' for the current lap
  , stopwatchLaps  :: [NominalDiffTime]
  -- ^ The times of previous laps (most recent first)
  } deriving (Eq, Show)

-- | Lens for the 'stopwatchTimer' attribute
stopwatchTimerL :: Lens' Stopwatch Timer
stopwatchTimerL = lens stopwatchTimer $
  \sw t -> sw { stopwatchTimer = t }

-- | Lens for the 'stopwatchLaps' attribute
stopwatchLapsL :: Lens' Stopwatch [NominalDiffTime]
stopwatchLapsL = lens stopwatchLaps $
  \sw l -> sw { stopwatchLaps = l }

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
startTimer timer = startTimerAt
  <$> getCurrentTime
  <*> return timer

-- | Stops a 'Timer'
stopTimer
  :: Timer
  -- ^ The 'Timer' being stopped
  -> IO Timer
  -- ^ The modified 'Timer'
stopTimer timer = stopTimerAt
  <$> getCurrentTime
  <*> return timer

-- | Calculates the amount of time elapsed on a 'Timer'
timeElapsed
  :: Timer
  -- ^ The 'Timer' being checked
  -> IO NominalDiffTime
  -- ^ The amount of time elapsed
timeElapsed timer = timeElapsedAt
  <$> getCurrentTime
  <*> return timer

-- | Starts a 'Countdown'
startCountdown
  :: Countdown
  -- ^ The 'Countdown' being started
  -> IO Countdown
  -- ^ Returns the modified 'Countdown'
startCountdown countdown = startCountdownAt
  <$> getCurrentTime
  <*> return countdown

-- | Stops a 'Countdown'
stopCountdown
  :: Countdown
  -- ^ The 'Countdown' being stopped
  -> IO Countdown
  -- ^ Returns the modified 'Countdown'
stopCountdown countdown = stopCountdownAt
  <$> getCurrentTime
  <*> return countdown

-- | Calculates the amount of time remaining in a 'Countdown'
timeRemaining
  :: Countdown
  -- ^ The 'Countdown' being checked
  -> IO NominalDiffTime
  -- ^ The amount of time remaining
timeRemaining countdown = timeRemainingAt
  <$> getCurrentTime
  <*> return countdown

-- | Determines whether or not a 'Countdown' has completed.
countdownIsCompleted
  :: Countdown
  -- ^ The 'Countdown' being checked
  -> IO Bool
  -- ^ Returns 'True' if the 'Countdown' has completed, 'False'
  -- otherwise
countdownIsCompleted countdown = countdownIsCompletedAt
  <$> getCurrentTime
  <*> return countdown

-- | Starts a 'Stopwatch'
startStopwatch
  :: Stopwatch
  -- ^ The 'Stopwatch' being started
  -> IO Stopwatch
  -- ^ Returns the modified 'Stopwatch'
startStopwatch stopwatch = startStopwatchAt
  <$> getCurrentTime
  <*> return stopwatch

-- | Stops a 'Stopwatch'
stopStopwatch
  :: Stopwatch
  -- ^ The 'Stopwatch' being stopped
  -> IO Stopwatch
  -- ^ Returns the modified 'Stopwatch'
stopStopwatch stopwatch = stopStopwatchAt
  <$> getCurrentTime
  <*> return stopwatch

-- | Starts a new lap
newLap
  :: Stopwatch
  -- ^ The 'Stopwatch' being modified
  -> IO Stopwatch
  -- ^ Returns the 'Stopwatch' being modified
newLap stopwatch = newLapAt
  <$> getCurrentTime
  <*> return stopwatch

-- | Returns the time of the current lap from a 'Stopwatch'
currentLap
  :: Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> IO NominalDiffTime
  -- ^ Returns the amount of time elapsed in the current lap
currentLap stopwatch = currentLapAt
  <$> getCurrentTime
  <*> return stopwatch

-- | Returns the lap times for a 'Stopwatch'
allLaps
  :: Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> IO [NominalDiffTime]
  -- ^ Returns the lap times (most recent first)
allLaps stopwatch = allLapsAt
  <$> getCurrentTime
  <*> return stopwatch

-- | Calculates the total run time of a 'Stopwatch'
totalStopwatchTime
  :: Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> IO NominalDiffTime
  -- ^ Returns the total run time
totalStopwatchTime stopwatch = totalStopwatchTimeAt
  <$> getCurrentTime
  <*> return stopwatch

-- | Converts a 'NominalDiffTime' to a 'TimeParts' value
decomposeNDT :: NominalDiffTime -> TimeParts
decomposeNDT t = TimeParts
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
composeNDT :: TimeParts -> NominalDiffTime
composeNDT tp = fromInteger millis / 1000
  where
    millis  = seconds * 1000 + toInteger (tpMillis tp)
    seconds = minutes * 60 + toInteger (tpSeconds tp)
    minutes = hours * 60 + toInteger (tpMinutes tp)
    hours   = toInteger (tpDays tp) * 24 + toInteger (tpHours tp)

-- | Converts a 'NominalDiffTime' into a more human-readable format
humanNDT :: NominalDiffTime -> String
humanNDT t = sign ++ d ++ h ++ m ++ s ++ ms
  where
    sign    = if t < 0 then "-" else ""
    d       = show (tpDays tp) ++ "d "
    h       = fix 2 (tpHours tp) ++ "h "
    m       = fix 2 (tpMinutes tp) ++ "m "
    s       = fix 2 (tpSeconds tp) ++ "."
    ms      = fix 3 (tpMillis tp) ++ "s"
    tp      = decomposeNDT $ abs t
    fix n x = let
      str  = show x
      slen = length str
      plen = n - slen
      pad  = replicate plen '0'
      in pad ++ str

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
startTimerAt
  :: UTCTime
  -- ^ The current time
  -> Timer
  -- ^ The 'Timer' being started
  -> Timer
  -- ^ The modified 'Timer'
startTimerAt t timer
  | isNothing (timerStartTime timer) =
    timer { timerStartTime = Just t }
  | otherwise = timer

-- | Stops a 'Timer' from a given time
stopTimerAt
  :: UTCTime
  -- ^ The current time
  -> Timer
  -- ^ The 'Timer' being stopped
  -> Timer
  -- ^ The modified 'Timer'
stopTimerAt t timer = newTimer { timerOffset = offset }
  where offset = timeElapsedAt t timer

-- | Calculates the amount of time elapsed on a 'Timer' from a given
-- time
timeElapsedAt
  :: UTCTime
  -- ^ The current time
  -> Timer
  -- ^ The 'Timer' being checked
  -> NominalDiffTime
  -- ^ The amount of time elapsed
timeElapsedAt t timer = case timerStartTime timer of
  Nothing -> timerOffset timer
  Just st -> timerOffset timer + diffUTCTime t st

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

-- | Starts a 'Countdown' using a given time
startCountdownAt
  :: UTCTime
  -- ^ The current time
  -> Countdown
  -- ^ The 'Countdown' being started
  -> Countdown
  -- ^ The modified 'Countdown'
startCountdownAt t countdown =
  countdown { countdownTimer = timer' }
  where
    timer' = startTimerAt t timer
    timer  = countdownTimer countdown

-- | Stops a 'Countdown' using a given time
stopCountdownAt
  :: UTCTime
  -- ^ The current time
  -> Countdown
  -- ^ The 'Countdown' being stopped
  -> Countdown
  -- ^ The modified 'Countdown'
stopCountdownAt t countdown =
  countdown { countdownTimer = timer' }
  where
    timer' = stopTimerAt t timer
    timer  = countdownTimer countdown

-- | Calculates the amount of time remaining in a 'Countdown' at a
-- given time
timeRemainingAt
  :: UTCTime
  -- ^ The current time
  -> Countdown
  -- ^ The 'Countdown' being checked
  -> NominalDiffTime
  -- ^ The amount of time remaining
timeRemainingAt t countdown = len - timeElapsedAt t timer
  where
    len   = countdownLength countdown
    timer = countdownTimer countdown

-- | Determines if a 'Countdown' is completed at a given time
countdownIsCompletedAt
  :: UTCTime
  -- ^ The current time
  -> Countdown
  -- ^ The 'Countdown' being checked
  -> Bool
  -- ^ Returns 'True' if the 'Countdown' has completed, 'False'
  -- otherwise.
countdownIsCompletedAt t countdown =
  timeRemainingAt t countdown <= 0

-- | Determines whether or not a 'Stopwatch' is running
stopwatchIsRunning
  :: Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> Bool
  -- ^ 'True' if the 'Stopwatch' is running, 'False' otherwise
stopwatchIsRunning = timerIsRunning . stopwatchTimer

-- | Determines whether or not a stopwatch has been started (even if
-- subsequently stopped)
stopwatchIsStarted
  :: Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> Bool
  -- ^ 'True' if started, 'False' otherwise
stopwatchIsStarted = (/= newStopwatch)

-- | Starts a 'Stopwatch' at a given time
startStopwatchAt
  :: UTCTime
  -- ^ The current time
  -> Stopwatch
  -- ^ The 'Stopwatch' being started
  -> Stopwatch
  -- ^ The modified 'Stopwatch'
startStopwatchAt t stopwatch = stopwatch { stopwatchTimer = timer }
  where timer = startTimerAt t $ stopwatchTimer stopwatch

-- | Stops a 'Stopwatch' at a given time
stopStopwatchAt
  :: UTCTime
  -- ^ The current time
  -> Stopwatch
  -- ^ The 'Stopwatch' being stopped
  -> Stopwatch
  -- ^ The modified 'Stopwatch'
stopStopwatchAt t stopwatch = stopwatch { stopwatchTimer = timer }
  where timer = stopTimerAt t $ stopwatchTimer stopwatch

-- | Starts a new lap at a given time
newLapAt
  :: UTCTime
  -- ^ The currebt time
  -> Stopwatch
  -- ^ The 'Stopwatch' being modified
  -> Stopwatch
  -- ^ The modified 'Stopwatch'
newLapAt t stopwatch = Stopwatch
  { stopwatchTimer = startTimerAt t newTimer
  , stopwatchLaps  = allLapsAt t stopwatch
  }

-- | Determines the length of the current lap given a time
currentLapAt
  :: UTCTime
  -- ^ The current time
  -> Stopwatch
  -- ^ The 'Stopwatch' being checkwd
  -> NominalDiffTime
  -- ^ The elapsed time for the current lap
currentLapAt t stopwatch = timeElapsedAt t timer
  where timer = stopwatchTimer stopwatch

-- | Returns the lap times from a 'Stopwatch' given a time
allLapsAt
  :: UTCTime
  -- ^ The current time
  -> Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> [NominalDiffTime]
  -- ^ The lap times (most recent first)
allLapsAt t stopwatch = currentLapAt t stopwatch :
  stopwatchLaps stopwatch

-- | Calculates the total runtime of a 'Stopwatch' given a time
totalStopwatchTimeAt
  :: UTCTime
  -- ^ The current time
  -> Stopwatch
  -- ^ The 'Stopwatch' being checked
  -> NominalDiffTime
  -- ^ The total run time
totalStopwatchTimeAt t stopwatch =
  sum $ allLapsAt t stopwatch

-- jl
