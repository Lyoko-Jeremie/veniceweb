#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname buildview -setcookie woomsgprivatecookie 
