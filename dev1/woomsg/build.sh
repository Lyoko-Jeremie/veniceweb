#!/bin/sh
cd `dirname $0`
exec erl -noshell -s make all -s erlang halt
