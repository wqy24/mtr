#!/bin/sh
# run-engine.sh - run chariot-engine.scm with example.chm and append (exit)

cat example.chm commands | gosh -r 7 -I . -I .. -I ./silver/ chariot-engine.scm > a.out
