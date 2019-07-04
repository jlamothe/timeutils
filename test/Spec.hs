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

import Data.Time.Clock (addUTCTime, getCurrentTime)

import Data.Time.Utils

main :: IO ()
main = hspec $ do
  timeElapsedUsingSpec
  startTimerUsingSpec
  stopTimerUsingSpec
  decomposeTimeSpec
  composeTimeSpec
  timerIsRunningSpec
  timeRemainingUsingSpec
  countdownIsCompletedUsingSpec
  countdownIsRunningSpec

timeElapsedUsingSpec :: Spec
timeElapsedUsingSpec = describe "timeElapsedUsing" $ do

  context "newTimer" $
    it "should be 0" $ do
      t <- getCurrentTime
      timeElapsedUsing t newTimer `shouldBe` 0

  context "started, no previous" $
    it "should be 1 minute" $ do
      t1 <- getCurrentTime
      let
        t2 = addUTCTime 60 t1
        timer = newTimer { timerStartTime = Just t1 }
      timeElapsedUsing t2 timer `shouldBe` 60

  context "stopped, with previous" $
    it "should be 1 minute" $ do
      t <- getCurrentTime
      let timer = newTimer { timerOffset = 60 }
      timeElapsedUsing t timer `shouldBe` 60

  context "started, with previous" $
    it "should be 2 minutes" $ do
      t1 <- getCurrentTime
      let
        t2 = addUTCTime 60 t1
        timer = newTimer
          { timerOffset    = 60
          , timerStartTime = Just t1
          }
      timeElapsedUsing t2 timer `shouldBe` 120

startTimerUsingSpec :: Spec
startTimerUsingSpec = describe "startTimerUsing" $ do

  context "newTimer" $
    it "should set the start time" $ do
      t <- getCurrentTime
      let expected = newTimer { timerStartTime = Just t }
      startTimerUsing t newTimer `shouldBe` expected

  context "already started" $
    it "should not modify the timer" $ do
      t1 <- getCurrentTime
      let
        t2 = addUTCTime 60 t1
        timer = newTimer { timerStartTime = Just t1 }
      startTimerUsing t2 timer `shouldBe` timer

  context "stopped" $
    it "should resume the timer" $ do
      t <- getCurrentTime
      let
        timer = newTimer { timerOffset = 60 }
        expected = timer { timerStartTime = Just t }
      startTimerUsing t timer `shouldBe` expected

stopTimerUsingSpec :: Spec
stopTimerUsingSpec = describe "stopTimerUsing" $ do

  context "newTimer" $
    it "should not change the timer" $ do
      t <- getCurrentTime
      stopTimerUsing t newTimer `shouldBe` newTimer

  context "stopped timer" $
    it "should not change the timer" $ do
      t <- getCurrentTime
      let timer = newTimer { timerOffset = 60 }
      stopTimerUsing t timer `shouldBe` timer

  context "running timer" $
    it "should stop the timer, and calculate the new offset" $ do
      t1 <- getCurrentTime
      let
        timer = newTimer
          { timerOffset    = 60
          , timerStartTime = Just t1
          }
        t2 = addUTCTime 60 t1
        expected = newTimer { timerOffset = 120 }
      stopTimerUsing t2 timer `shouldBe` expected

decomposeTimeSpec :: Spec
decomposeTimeSpec = describe "decomposeTime" $ do
  let
    days    = 1
    hours   = days * 24 + 2
    minutes = hours * 60 + 3
    seconds = minutes * 60 + 4
    millis  = seconds * 1000 + 5
    time    = millis / 1000

  context "zero" $
    it "should be zero" $
      decomposeTime 0 `shouldBe` newTimeParts

  context "positive" $
    it "should calculate the time parts" $ let
      expected = TimeParts
        { tpDays    = 1
        , tpHours   = 2
        , tpMinutes = 3
        , tpSeconds = 4
        , tpMillis  = 5
        }
      in decomposeTime time `shouldBe` expected

  context "negative" $
    it "should calculate the time parts" $ let
      expected = TimeParts
        { tpDays    = -1
        , tpHours   = -2
        , tpMinutes = -3
        , tpSeconds = -4
        , tpMillis  = -5
        }
      in decomposeTime (-time) `shouldBe` expected

composeTimeSpec :: Spec
composeTimeSpec = describe "composeTime" $
  it "should compose correctly" $ let
    parts = TimeParts
      { tpDays    = 1
      , tpHours   = 2
      , tpMinutes = 3
      , tpSeconds = 4
      , tpMillis  = 5
      }
    t = composeTime parts
    in decomposeTime t `shouldBe` parts

timerIsRunningSpec :: Spec
timerIsRunningSpec = describe "timerIsRunning" $ do

  context "not running" $
    it "should return False" $
      timerIsRunning newTimer `shouldBe` False

  context "running" $
    it "should return True" $ do
      timer <- startTimer newTimer
      timerIsRunning timer `shouldBe` True

timeRemainingUsingSpec :: Spec
timeRemainingUsingSpec = describe "timeRemainingUsing" $ do

  context "not started" $
    it "should be one minute" $ do
      t <- getCurrentTime
      let countdown = newCountdown 60
      timeRemainingUsing t countdown `shouldBe` 60

  context "started" $ mapM_
    (\(len, dt, expected) -> let
      label = "length: " ++ show len ++
        ", elapsed: " ++ show dt
      expectation = "should be " ++ show expected
      in
        context label $
          it expectation $ do
            t1 <- getCurrentTime
            let
              t2        = addUTCTime dt t1
              timer     = startTimerUsing t1 newTimer
              countdown = Countdown
                { countdownLength = len
                , countdownTimer  = timer
              }
            timeRemainingUsing t2 countdown `shouldBe` expected)
    --  length, elapsed, expected
    [ ( 60,     30,      30       )
    , ( 60,     60,      0        )
    , ( 30,     60,      -30      )
    ]

countdownIsCompletedUsingSpec :: Spec
countdownIsCompletedUsingSpec = describe "countdownIsCompletedUsing" $ do

  context "not started" $ mapM_
    (\(label, dt, expected) ->
      context label $
        it ("should be " ++ show expected) $ do
          t <- getCurrentTime
          let cd = newCountdown dt
          countdownIsCompletedUsing t cd `shouldBe` expected)
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
            timer     = startTimerUsing t1 newTimer
            countdown = (newCountdown len) { countdownTimer = timer }
          countdownIsCompletedUsing t2 countdown `shouldBe` expected)
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

-- jl
