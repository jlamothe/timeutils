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

import Test.Hspec

import Data.Time.Clock (addUTCTime, getCurrentTime)

import Data.Time.Utils

main :: IO ()
main = hspec $ do
  timeElapsedUsingSpec
  startTimerUsingSpec
  stopTimerUsingSpec

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

-- jl
