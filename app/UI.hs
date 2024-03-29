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

module UI (draw, mkAttrMap) where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Types (Widget)
import Brick.Util (fg, on)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (hBox, str, vBox, withAttr)
import qualified Graphics.Vty.Attributes as A

import Data.Time.Utils
import Types

draw :: ProgState -> [Widget ()]
draw s = [center $ case progMode s of
    StopwatchMode -> stopwatchW s
    CountdownMode -> countdownsW s]

mkAttrMap :: ProgState -> AttrMap
mkAttrMap _ = attrMap
  (A.green `on` A.black)
  [ ( titleAttr
    , A.currentAttr `A.withStyle` (A.bold + A.reverseVideo)
    )
  , ( labelAttr
    , A.currentAttr `A.withStyle` A.bold
    )
  , ( completedAttr
    , fg A.red `A.withStyle` A.bold
    )
  ]

stopwatchW :: ProgState -> Widget ()
stopwatchW s = vBox $
  [ withAttr titleAttr $ str "Stopwatch"
  , hBox
    [ withAttr labelAttr $ str "Total time: "
    , str $ humanNDT $ totalStopwatchTimeAt t sw
    ]
  , withAttr labelAttr $ str "Laps:"
  ] ++ map (str . humanNDT) (allLapsAt t sw)
  where
    t  = currentTime s
    sw = stopwatch s

countdownsW :: ProgState -> Widget ()
countdownsW s = vBox $
  withAttr titleAttr (str "Countdowns") :
  if null cds
    then [str "(no countdowns)"]
    else map (uncurry display) cds
  where
    t            = currentTime s
    cds          = zip [0..] $ countdowns s
    display i cd = let
      selected = countdownSel s == Just i
      left     = str $ if selected then ">" else " "
      right    = str $ if selected then "<" else " "
      subW     = str $ humanNDT $ timeRemainingAt t cd
      mainW    = if countdownIsCompletedAt t cd
        then withAttr completedAttr subW
        else subW
      in hBox [left, mainW, right]

titleAttr :: AttrName
titleAttr = attrName "title"

labelAttr :: AttrName
labelAttr = attrName "label"

completedAttr :: AttrName
completedAttr = attrName "completed"

-- jl
