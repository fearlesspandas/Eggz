#!/usr/bin/env bash


export PHYSICS_ADDR=ws://127.0.0.1:8081
export RANDOMIZED_SPAWN_COUNT=2000
export WORLDBLOCK_RADIUS=32674
export PROWLER_COUNT=0
export PHYSICS_SOCKET_UPDATE_INTERVAL=1000 #milliseconds
#export SBT_OPTS='-Xmx2g'
sbt run 
