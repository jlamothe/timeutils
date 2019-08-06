# timeutils

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

## Executive Summary

This package contains some simple time-based tools, and consists of
two parts: a library, and an executable.  The library should be fairly
portable, and provides some simple timer/stopwatch functionality.  The
executanle is a simple console-based program built on top of the
[brick](http://hackage.haskell.org/package/brick) package, and as
such, will presently only work on POSIX-compatible systems (sorry
Windows users).

## Library

The library provides its functionalty through the `Data.Time.Utils`
module, and is thoroughlly documented [on
Hackage](http://hackage.haskell.org/package/timeutils).

## Executable

This package provides the `timeutils` executable, a console-based
program with a rudimentary user interface.  It has two modes:
stopwatch and countdown.  You can toggle between these modes by
pressing the `<tab>` key. The program can be exited with the `<esc>`
key, the `q` key, or by hitting `<ctrl-c>`.

### Stopwatch Mode

The following is a list of keys and their effects in stopwatch mode:

Key | Effect
--- | ------
`<space>` | start/stop stopwatch
`l` | start new lap
`r` | reset stopwatch

### Countdown Mode

The following is a list of keys and their effects in countdown mode:

Key | Effect
--- | ------
`n` | new countdown
`<up>`/`<down>` | select countdown
`<space>` | start/stop selected countdown
`<del>` | delete selected countdown
`r` | reset selected countdown
`R` | delete all countdowns
`d`/`D` | increment/deincrement days in current countdown
`h`/`H` | increment/deincrement hours in current countdown
`m`/`M` | increment/deincrement minutes in current countdown
`s`/`S` | increment/deincrement seconds in current countdown
