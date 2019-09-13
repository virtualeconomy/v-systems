#!/bin/bash

echo "[-----INFO-----] Wallet data file: $1/wallet.dat"
sbt clean
echo "[-----INFO-----] Old target cleaned!"
sbt -Dnetwork=testnet packageAll
echo "[-----INFO-----] Project compiled!"
sbt it:test
echo "[-----INFO-----] Docker file completed!"

echo "[-----INFO-----] Stopping old containers..."
docker stop peer_container
docker stop minter_container

docker rm peer_container
docker rm minter_container

docker rmi peer_node_img
docker build -t peer_node_img target/docker
echo "[-----INFO-----] Image for peer node built!"

if [ -x minter_target ]; then
	rm -rf minter_target
fi
mkdir minter_target
mkdir minter_target/docker

cp -a target/docker/. minter_target/docker

if [[ -n $2 && $2 != "" ]]; then
	IP_PORT=$2:19923
else
	IP_PORT=172.17.0.2:9923
fi
echo $IP_PORT

ORIGIN_PEER="known-peers = \[\]"
NEW_PEER="known-peers = [\"$IP_PORT\"]"
sed -i "" "s|$ORIGIN_PEER|$NEW_PEER|" minter_target/docker/1/template.conf
cp $1/wallet.dat minter_target/docker/1
gsed -i "s|RUN|ADD 1/wallet.dat /tmp/vsys/wallet/\n&|" minter_target/docker/Dockerfile
docker rmi minter_node_img
docker build -t minter_node_img minter_target/docker
echo "[-----INFO-----] Image for minter node built!"

echo "[-----INFO-----] Starting peer node..."
osascript -e 'tell app "Terminal" to do script "docker run --name peer_container -p 19923:9923 -p 19922:9922 peer_node_img -it sh"'
echo "[-----INFO-----] Peer node started!"
echo "[-----INFO-----] Starting minter node..."
osascript -e 'tell app "Terminal" to do script "docker run --name minter_container -p 9923:9923 -p 9922:9922 minter_node_img -it sh"'
echo "[-----INFO-----] Minter node started!"