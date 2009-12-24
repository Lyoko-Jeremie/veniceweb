#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config log.config -setcookie woomsgprivatecookie -sname $1 -mnesia dir $2 -s woomsg
