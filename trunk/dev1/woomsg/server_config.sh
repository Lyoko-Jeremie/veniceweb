#!/bin/sh
cd `dirname $0`
mkdir $1/mnesia
mkdir $1/mnesia/dbnodemaster
mkdir $1/mnesia/dbnodeslave1
mkdir $1/mnesia/dbnodeslave2
mkdir $1/mnesia/dbnodeslave3
mkdir $1/mnesia/dbnodeslave4
mkdir $1/error_logs
mkdir $1/error_logs_dev
