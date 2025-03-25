#!/bin/bash

export PROFILE="SERVER"
export HOST="$1"
export PHYSICS_HOST="$2"

export PHYSICS_ADDR=ws://127.0.0.1:8081
export WORLDBLOCK_RADIUS=32768
export RANDOMIZED_SPAWN_COUNT=200000
export PROWLER_COUNT=50

echo "retrieving deploy script" & curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/build_server.sh > build_server.sh
echo "retrieving physics socket" & curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/physics_socket_binary > physics_socket
echo "retrieving Eggz game server" & curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/Eggz.x86_64 > Eggz
echo "retrieving game socket library" & curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/libClientPhysicsSocket.so > libClientPhysicsSocket.so
echo "retrieving game client libraries" & curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/libNativeTools.so > libNativeTools.so
echo "retrieving eggz scala" & curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/eggz-assembly-0.1.0-SNAPSHOT.jar > eggz-server.jar

echo "Done retrieving files"


chmod +x physics_socket
chmod +x eggz-server.jar
chmod +x Eggz

echo "Starting Servers"

./physics_socket $PHYSICS_HOST > physics_out &&\
	java -jar eggz-server.jar > server_out &&\
	./Eggz > game_out

echo "Server components Started"
