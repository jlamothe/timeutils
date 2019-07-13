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

import Test.Hspec (Spec, context, describe, hspec, it, shouldBe)

import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime
  , addUTCTime
  , getCurrentTime
  )

import Data.Time.Utils

main :: IO ()
main = hspec $ do
  timeElapsedAtSpec
  startTimerAtSpec
  stopTimerAtSpec
  decomposeNDTSpec
  composeNDTSpec
  timerIsRunningSpec
  timerIsStartedSpec
  timeRemainingAtSpec
  countdownIsCompletedAtSpec
  countdownIsRunningSpec
  countdownIsStartedSpec
  startCountdownSpec
  stopCountdownSpec
  currentLapAtSpec
  allLapsAtSpec
  totalStopwatchTimeAtSpec
  stopwatchIsRunningSpec
  stopwatchIsStartedSpec
  startStopwatchSpec
  stopStopwatchSpec
  newLapAtSpec
  humanNDTSpec

timeElapsedAtSpec :: Spec
timeElapsedAtSpec = describe "timeElapsedAt" $ do

  context "newTimer" $
    it "should be 0" $ do
      t <- getCurrentTime
      timeElapsedAt t newTimer `shouldBe` 0

  context "started, no previous" $
    it "should be 1 minute" $ do
      (t, t') <- times 60
      let timer = newTimer { timerStartTime = Just t }
      timeElapsedAt t' timer `shouldBe` 60

  context "stopped, with previous" $
    it "should be 1 minute" $ do
      t <- getCurrentTime
      let timer = newTimer { timerOffset = 60 }
      timeElapsedAt t timer `shouldBe` 60

  context "started, with previous" $
    it "should be 2 minutes" $ do
      (t, t') <- times 60
      let
        timer = newTimer
          { timerOffset    = 60
          , timerStartTime = Just t
          }
      timeElapsedAt t' timer `shouldBe` 120

startTimerAtSpec :: Spec
startTimerAtSpec = describe "startTimerAt" $ do

  context "newTimer" $
    it "should set the start time" $ do
      t <- getCurrentTime
      let expected = newTimer { timerStartTime = Just t }
      startTimerAt t newTimer `shouldBe` expected

  context "already started" $
    it "should not modify the timer" $ do
      (t, t') <- times 60
      let timer = newTimer { timerStartTime = Just t }
      startTimerAt t' timer `shouldBe` timer

  context "stopped" $
    it "should resume the timer" $ do
      t <- getCurrentTime
      let
        timer = newTimer { timerOffset = 60 }
        expected = timer { timerStartTime = Just t }
      startTimerAt t timer `shouldBe` expected

stopTimerAtSpec :: Spec
stopTimerAtSpec = describe "stopTimerAt" $ do

  context "newTimer" $
    it "should not change the timer" $ do
      t <- getCurrentTime
      stopTimerAt t newTimer `shouldBe` newTimer

  context "stopped timer" $
    it "should not change the timer" $ do
      t <- getCurrentTime
      let timer = newTimer { timerOffset = 60 }
      stopTimerAt t timer `shouldBe` timer

  context "running timer" $
    it "should stop the timer, and calculate the new offset" $ do
      (t, t') <- times 60
      let
        timer = newTimer
          { timerOffset    = 60
          , timerStartTime = Just t
          }
        expected = newTimer { timerOffset = 120 }
      stopTimerAt t' timer `shouldBe` expected

decomposeNDTSpec :: Spec
decomposeNDTSpec = describe "decomposeNDT" $ do
  let
    days    = 1
    hours   = days * 24 + 2
    minutes = hours * 60 + 3
    seconds = minutes * 60 + 4
    millis  = seconds * 1000 + 5
    time    = millis / 1000

  context "zero" $
    it "should be zero" $
      decomposeNDT 0 `shouldBe` newTimeParts

  context "positive" $
    it "should calculate the time parts" $ let
      expected = TimeParts
        { tpDays    = 1
        , tpHours   = 2
        , tpMinutes = 3
        , tpSeconds = 4
        , tpMillis  = 5
        }
      in decomposeNDT time `shouldBe` expected

  context "negative" $
    it "should calculate the time parts" $ let
      expected = TimeParts
        { tpDays    = -1
        , tpHours   = -2
        , tpMinutes = -3
        , tpSeconds = -4
        , tpMillis  = -5
        }
      in decomposeNDT (-time) `shouldBe` expected

composeNDTSpec :: Spec
composeNDTSpec = describe "composeNDT" $
  it "should compose correctly" $ let
    parts = TimeParts
      { tpDays    = 1
      , tpHours   = 2
      , tpMinutes = 3
      , tpSeconds = 4
      , tpMillis  = 5
      }
    t = composeNDT parts
    in decomposeNDT t `shouldBe` parts

timerIsRunningSpec :: Spec
timerIsRunningSpec = describe "timerIsRunning" $ do

  context "not running" $
    it "should return False" $
      timerIsRunning newTimer `shouldBe` False

  context "running" $
    it "should return True" $ do
      timer <- startTimer newTimer
      timerIsRunning timer `shouldBe` True

timerIsStartedSpec :: Spec
timerIsStartedSpec = describe "timerIsStarted" $ do

  context "newTimer" $
    it "should be False" $
      timerIsStarted newTimer `shouldBe` False

  context "started" $
    it "should be True" $ do
      timer <- startTimer newTimer
      timerIsStarted timer `shouldBe` True

  context "started, then stopped" $
    it "should be True" $ do
      (t, t') <- times 60
      let
        timer  = startTimerAt t newTimer
        timer' = stopTimerAt t' timer
      timerIsStarted timer' `shouldBe` True

timeRemainingAtSpec :: Spec
timeRemainingAtSpec = describe "timeRemainingAt" $ do

  context "not started" $
    it "should be one minute" $ do
      t <- getCurrentTime
      let countdown = newCountdown 60
      timeRemainingAt t countdown `shouldBe` 60

  context "started" $ mapM_
    (\(len, dt, expected) -> let
      label = "length: " ++ show len ++
        ", elapsed: " ++ show dt
      expectation = "should be " ++ show expected
      in
        context label $
          it expectation $ do
            (t, t') <- times dt
            let
              timer     = startTimerAt t newTimer
              countdown = Countdown
                { countdownLength = len
                , countdownTimer  = timer
              }
            timeRemainingAt t' countdown `shouldBe` expected)
    --  length, elapsed, expected
    [ ( 60,     30,      30       )
    , ( 60,     60,      0        )
    , ( 30,     60,      -30      )
    ]

countdownIsCompletedAtSpec :: Spec
countdownIsCompletedAtSpec = describe "countdownIsCompletedAt" $ do

  context "not started" $ mapM_
    (\(label, dt, expected) ->
      context label $
        it ("should be " ++ show expected) $ do
          t <- getCurrentTime
          let cd = newCountdown dt
          countdownIsCompletedAt t cd `shouldBe` expected)
    --  label,               length, expected
    [ ( "negative length",   -60,    True     )
    , ( "zero length",       0,      True     )
    , ( "posititive length", 60,     False    )
    ]

  context "started" $ mapM_
    (\(len, dt, expected) -> let
      label = "length: " ++ show len ++
        ", elapsed: " ++ show dt
      in context label $
        it ("should be " ++ show expected) $ do
          t1 <- getCurrentTime
          let
            t2        = addUTCTime dt t1
            timer     = startTimerAt t1 newTimer
            countdown = (newCountdown len) { countdownTimer = timer }
          countdownIsCompletedAt t2 countdown `shouldBe` expected)
    --  length, elapsed, expected
    [ ( 60,     0,       False    )
    , ( 60,     30,      False    )
    , ( 60,     60,      True     )
    , ( 0,      0,       True     )
    , ( 30,     60,      True     )
    ]

countdownIsRunningSpec :: Spec
countdownIsRunningSpec = describe "countdownIsRunning" $ do

  context "not running" $
    it "should return False" $ let
      countdown = newCountdown 60
      in countdownIsRunning countdown `shouldBe` False

  context "running" $
    it "should return True" $ do
      timer <- startTimer newTimer
      let countdown = (newCountdown 60) { countdownTimer = timer }
      countdownIsRunning countdown `shouldBe` True

countdownIsStartedSpec :: Spec
countdownIsStartedSpec = describe "countdownIsStarted" $ do

  context "new countdown" $
    it "should be False" $
      countdownIsStarted (newCountdown 60) `shouldBe` False

  context "countdown started" $
    it "should be True" $ do
      countdown <- startCountdown $ newCountdown 60
      countdownIsStarted countdown `shouldBe` True

  context "countdown started, then stopped" $
    it "should be True" $ do
      (t, t') <- times 30
      let
        countdown  = startCountdownAt t $ newCountdown 60
        countdown' = stopCountdownAt t' countdown
      countdownIsStarted countdown' `shouldBe` True

startCountdownSpec :: Spec
startCountdownSpec = describe "startCountdown" $
  it "should start the countdown" $ do
    let countdown = newCountdown 60
    countdown' <- startCountdown countdown
    countdownIsRunning countdown' `shouldBe` True

stopCountdownSpec :: Spec
stopCountdownSpec = describe "stopCountdown" $
  it "should stop the countdown" $ do
    countdown  <- startCountdown $ newCountdown 60
    countdown' <- stopCountdown countdown
    countdownIsRunning countdown' `shouldBe` False

currentLapAtSpec :: Spec
currentLapAtSpec = describe "currentLapAt" $ do

  context "newStopwatch" $
    it "should be zero" $ do
      t <- getCurrentTime
      currentLapAt t newStopwatch ` shouldBe` 0

  context "running" $
    it "should be 1 minute" $ do
      (t, t') <- times 60
      let stopwatch = runningStopwatch t
      currentLapAt t' stopwatch `shouldBe` 60

allLapsAtSpec :: Spec
allLapsAtSpec = describe "allLapsAt" $ do

  context "newStopwatch" $
    it "should return a lap of zero length" $ do
      t <- getCurrentTime
      allLapsAt t newStopwatch `shouldBe` [0]

  context "single lap" $
    it "should return a single lap of 1 minute" $ do
      (t, t') <- times 60
      let stopwatch = runningStopwatch t
      allLapsAt t' stopwatch `shouldBe` [60]

  context "multiple laps" $
    it "should return the lap times" $ do
      (t, t') <- times 60
      let
        timer     = startTimerAt t newTimer
        stopwatch = Stopwatch
          { stopwatchTimer = timer
          , stopwatchLaps  = [10, 20, 30]
          }
      allLapsAt t' stopwatch `shouldBe` [60, 10, 20, 30]

totalStopwatchTimeAtSpec :: Spec
totalStopwatchTimeAtSpec = describe "totalStopwatchTimeAt" $ do

  context "newStopwatch" $
    it "should be zero" $ do
      t <- getCurrentTime
      totalStopwatchTimeAt t newStopwatch `shouldBe` 0

  context "single lap" $
    it "should be 1 minute" $ do
      (t, t') <- times 60
      let stopwatch = runningStopwatch t
      totalStopwatchTimeAt t' stopwatch `shouldBe` 60

  context "multiple laps" $
    it "should be 90 seconds" $ do
      (t, t') <- times 60
      let
        stopwatch = (runningStopwatch t)
          { stopwatchLaps = [10, 20] }
      totalStopwatchTimeAt t' stopwatch `shouldBe` 90

stopwatchIsRunningSpec :: Spec
stopwatchIsRunningSpec = describe "stopwatchIsRunning" $ do

  context "newStopwatch" $
    it "should be False" $
      stopwatchIsRunning newStopwatch `shouldBe` False

  context "running" $
    it "should be True" $ do
      t <- getCurrentTime
      let stopwatch = runningStopwatch t
      stopwatchIsRunning stopwatch `shouldBe` True

stopwatchIsStartedSpec :: Spec
stopwatchIsStartedSpec = describe "stopwatchIsStarted" $ do

  context "newStopwatch" $
    it "should be False" $
      stopwatchIsStarted newStopwatch `shouldBe` False

  context "started" $
    it "should be True" $ do
      (t, t') <- times 60
      let
        timer     = startTimerAt t newTimer
        stopwatch = newStopwatch { stopwatchTimer = timer }
      stopwatchIsStarted stopwatch `shouldBe` True

  context "previous laps" $
    it "should be True" $ let
      stopwatch = newStopwatch { stopwatchLaps = [60] }
      in stopwatchIsStarted stopwatch `shouldBe` True

startStopwatchSpec = describe "startStopwatch" $
  it "should start the stopwatch" $ do
    stopwatch <- startStopwatch newStopwatch
    stopwatchIsRunning stopwatch `shouldBe` True

stopStopwatchSpec = describe "stopStopwatch" $
  it "should stop the Stopwatch" $ do
    stopwatch  <- startStopwatch newStopwatch
    stopwatch' <- stopStopwatch stopwatch
    stopwatchIsRunning stopwatch' `shouldBe` False

newLapAtSpec :: Spec
newLapAtSpec = describe "newLapAt" $ do

  context "newStopwatch" $
    it "should record a zero length lap, and start a new one" $ do
      (t, t') <- times 60
      let stopwatch = newLapAt t newStopwatch
      allLapsAt t' stopwatch `shouldBe` [60, 0]

  context "running" $
    it "should start a new lap" $ do
      (t, t') <- times 60
      let
        stopwatch  = runningStopwatch t
        stopwatch' = newLapAt t' stopwatch
      allLapsAt t' stopwatch' `shouldBe` [0, 60]

  context "previous lap" $
    it "should start a new lap" $ do
      (t, t') <- times 60
      let
        stopwatch  = (runningStopwatch t)
          { stopwatchLaps = [30] }
        stopwatch' = newLapAt t' stopwatch
      allLapsAt t' stopwatch' `shouldBe` [0, 60, 30]

humanNDTSpec :: Spec
humanNDTSpec = describe "humanNDT" $ mapM_
  (\(val, expected) ->
    context (show val) $
      it ("should be " ++ expected) $
        humanNDT val `shouldBe` expected)
  --  value,    expected
  [ ( 0,        "0d 00h 00m 00.000s"  )
  , ( single,   "1d 02h 03m 04.005s"  )
  , ( double,   "11d 12h 13m 14.015s" )
  , ( triple,   "11d 12h 13m 14.123s" )
  , ( negative, "-1d 02h 03m 04.005s" )
  ]
  where
    single   = composeNDT $ TimeParts 1 2 3 4 5
    double   = composeNDT $ TimeParts 11 12 13 14 15
    triple   = composeNDT $ TimeParts 11 12 13 14 123
    negative = -single

times :: NominalDiffTime -> IO (UTCTime, UTCTime)
times dt = do
  t <- getCurrentTime
  let t' = addUTCTime dt t
  return (t, t')

runningStopwatch :: UTCTime -> Stopwatch
runningStopwatch t = newStopwatch { stopwatchTimer = timer }
  where timer = startTimerAt t newTimer

-- jl
