#!/bin/bash


export PHYSICS_ADDR=ws://127.0.0.1:8081
export RANDOMIZED_SPAWN_COUNT=200000
export WORLDBLOCK_RADIUS=32674
export PROWLER_COUNT=30
#export SBT_OPTS='-Xmx2g'
sbt run 
