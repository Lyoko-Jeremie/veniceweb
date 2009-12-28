#!/bin/sh
cd `dirname $0`
echo "create mnesia cluster"
mkdir $1/mnesia
mkdir $1/mnesia/dbnodemaster
mkdir $1/mnesia/dbnodeslave1
mkdir $1/mnesia/dbnodeslave2
mkdir $1/mnesia/dbnodeslave3
mkdir $1/mnesia/dbnodeslave4
mkdir $1/mnesia/servernode

echo "create error logs"
mkdir $1/error_logs
mkdir $1/error_logs_dev

echo "create fs"
mkdir $1/fs

echo "create photo"
mkdir $1/fs/photo
mkdir $1/fs/photo/node1
mkdir $1/fs/photo/node2
mkdir $1/fs/photo/node3
mkdir $1/fs/photo/node4

echo "create photo/sys"
mkdir $1/fs/photo/sys
mkdir $1/fs/photo/sys/path-guid
mkdir $1/fs/photo/sys/path-guid/mini
mkdir $1/fs/photo/sys/path-guid/normal

echo "create pic"
mkdir $1/fs/pic
mkdir $1/fs/pic/node1
mkdir $1/fs/pic/node2
mkdir $1/fs/pic/node3
mkdir $1/fs/pic/node4
echo "complete!"
