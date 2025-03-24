#!/bin/bash

export PROFILE="SERVER"
export HOST="$1"
export PHYSICS_HOST="$2"

export PHYSICS_ADDR=ws://127.0.0.1:8081
export WORLDBLOCK_RADIUS=32768
export RANDOMIZED_SPAWN_COUNT=200000
export PROWLER_COUNT=50

curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/build_server.sh > build_server.sh
curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/physics_socket_binary > physics_socket
curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/Eggz.x86_64 > Eggz
curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/libClientPhysicsSocket.so > libClientPhysicsSocket.so
curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/libNativeTools.so > libNativeTools.so
curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/eggz-assembly-0.1.0-SNAPSHOT.jar > eggz-server.jar

chmod +x physics_socket
chmod +x eggz-server.jar

./physics_socket $PHYSICS_HOST &&\
	java -jar eggz-server.jar &&\
	./Eggz
