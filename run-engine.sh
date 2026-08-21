# run-engine.sh -- this file is a development tool for CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
# Copyright (C) 2026 wqy24
# This file is under the same license as CHARIOT
#
# CHARIOT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# CHARIOT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with CHARIOT. If not, see <https://www.gnu.org/licenses/>.


cat example.chm commands | gosh -r 7 -I . -I .. -I ./silver/ chariot-engine.scm > a.out
# cat example.chm commands | gsi -:r7rs,search=.,search=..,search=../chibi-scheme/lib chariot-engine.scm > a.out
