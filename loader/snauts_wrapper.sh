#!/bin/sh

./lifter <$1 2>/dev/null &

snauts_pid=$!

sleep 4
kill -INT $snauts_pid 2>/dev/null

