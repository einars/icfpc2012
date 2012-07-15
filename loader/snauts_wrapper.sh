#!/bin/sh

./lifter <$1 2>/dev/null &

snauts_pid=$!

sleep 3
kill -INT $snauts_pid 2>/dev/null

