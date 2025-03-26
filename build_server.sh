#!/bin/bash

case "$1" in
	"get")
		case "$2" in
			"deploy")
				echo "retrieving deploy script" &\
					curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/build_server.sh > build_server.sh;;

			"client")
				echo "retrieving Eggz client" &\
					curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/Eggz.x86_64 > Eggz
				echo "retrieving Eggz pack" &\
					curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/Eggz.pck > Eggz.pck
				echo "retrieving game socket library" &\
					curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/libClientPhysicsSocket.so > libClientPhysicsSocket.so
				echo "retrieving game client libraries" &\
					curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/libNativeTools.so > libNativeTools.so
				chmod +x Eggz;;

			"server")
				echo "retrieving physics socket" &\
					curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/physics_socket_binary > physics_socket
				echo "retrieving eggz scala" &\
					curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/eggz-assembly-0.1.0-SNAPSHOT.jar > eggz-server.jar
				echo "retrieving Godot Server" &\
					curl -L https://github.com/fearlesspandas/EggzClientUI/releases/download/Eggz-test-deploy/Godot_v3.5.3-stable_linux_server.64 > godot_server
				chmod +x physics_socket
				chmod +x eggz-server.jar
				chmod +x godot_server;;

			*)
				echo "unrecognized argument for command:get $2";;
		esac
		echo "Done retrieving files";;

	"start")
		case "$2" in
			"client")
				if [ ! -f ./Eggz ]; then
					echo "Err:Eggz client not found!"
					exit
				fi
				if [ ! -f ./Eggz.pck ]; then
					echo "Err:Eggz.pck not found!"
					exit
				fi
				if [ ! -f ./libNativeTools.so ]; then
					echo "Err:Native libraries not found!"
					exit
				fi
				if [ ! -f ./libClientPhysicsSocket.so ]; then
					echo "Err:Physics socket client not found!"
					exit
				fi
				./Eggz;;

			"world")
				if [ ! -f ./Eggz ]; then
					echo "Err:Eggz client not found!"
					exit
				fi
				if [ ! -f ./Eggz.pck ]; then
					echo "Err:Eggz.pck not found!"
					exit
				fi
				if [ ! -f ./libNativeTools.so ]; then
					echo "Err:Native libraries not found!"
					exit
				fi
				if [ ! -f ./libClientPhysicsSocket.so ]; then
					echo "Err:Physics socket client not found!"
					exit
				fi
				if [ ! -f ./godot_server ]; then
					echo "Err:Godot Server not found!"
					exit
				fi
				export EGGZ_PROFILE="1" 
				export PROFILE="SERVER"
				export EGGZ_HOST=$(hostname -I | xargs)
				echo "Starting game world with physics server $EGGZ_HOST..."
				./godot_server --main-pack Eggz.pck > game_out &\
					echo "Game server started";;

			"server")
				if [ ! -f ./physics_socket ]; then
					echo "Err:physics_socket not found!"
					exit
				fi
				if [ ! -f ./eggz-server.jar ]; then
					echo "Err:server jar not found!"
					exit
				fi
				export PUBLIC_DNS="$3"
				export PHYSICS_ADDR=ws://$(hostname -I | xargs):8081
				echo "starting server with physics address $PHYSICS_ADDR"
				export WORLDBLOCK_RADIUS=32768
				export RANDOMIZED_SPAWN_COUNT=200000
				export PROWLER_COUNT=50
				echo "Starting Server components..."
				./physics_socket $PUBLIC_DNS > physics_out &\
					java -jar eggz-server.jar > server_out &\
					echo "Server components (server, physics server) Started";;
			*)
				echo "unrecognized argument for command:start $2";;
		esac;;
	*)
		echo "unrecognized argument $1";;
esac
