#!/bin/bash
touch genesis_settings.txt || exit

echo "[-----INFO-----] Generating Genesis Block Info!"
java -cp "target/vsys-all-0.1.1.jar:target/test-classes" tools.GenesisBlockGenerator > genesis_settings.txt

echo "[-----INFO-----] Updating template.conf!"

transactionMode=false
transactions=""
slot=0
while read line
do
    if [ $transactionMode = false ]
    then
        key=$(echo $line| cut -d ':' -f 1| tr -d ' ')
    else
        key=$(echo $line| cut -d '(' -f 1| tr -d ' ')
        if [ "$key" = "GenesisTransactionSettings" ]
        then
            transaction=$(echo $line| cut -d '(' -f 2)
            address=$(echo $transaction| cut -d ',' -f 1)
            amount=$(echo $transaction| cut -d ',' -f 2)
            # echo $address, $amount
            if [ $slot = 0 ]
            then
                transactions+="[{recipient = $address, amount = $amount, slot-id = $slot}"
            else
                transactions+=",{recipient = $address, amount = $amount, slot-id = $slot}"
            fi
            slot=$(( $slot + 1 ))
        else
            transactionMode=false
        fi
    fi

    if [ "$key" = "timestamp" ]
    then
        timestamp=$(echo $line| cut -d ':' -f 2)
    elif [ "$key" = "blockTimestamp" ]
    then
        blockTimestamp=$(echo $line| cut -d ':' -f 2)
    elif [ "$key" = "averageBlockDelay" ]
    then
        averageBlockDelay=$(echo $line| cut -d ':' -f 2)
        averageBlockDelay=$(echo $averageBlockDelay| cut -d ' ' -f 1)s
    elif [ "$key" = "initialBalance" ]
    then
        initialBalance=$(echo $line| cut -d ':' -f 2)
    elif [ "$key" = "initialMintTime" ]
    then
        initialMintTime=$(echo $line| cut -d ':' -f 2)
    elif [ "$key" = "signature" ]
    then
        signature=$(echo $line| cut -d ':' -f 2)
        signature=${signature:6:88}
    elif [ "$key" = "transactions" ]
    then
        transactionMode=true
    fi
done < genesis_settings.txt
transactions+="]"

sed -i -c "s/\(timestamp *=*\).*/\1$timestamp/" "src/it/resources/template.conf"
echo "[-----INFO-----] timestamp =$timestamp"
sed -i -c "s/\(block-timestamp *=*\).*/\1$blockTimestamp/" "src/it/resources/template.conf"
echo "[-----INFO-----] block-timestamp =$blockTimestamp"
sed -i -c "s/\(average-block-delay *=*\).*/\1$averageBlockDelay/" "src/it/resources/template.conf"
echo "[-----INFO-----] average-block-delay =$averageBlockDelay"
sed -i -c "s/\(initial-balance *=*\).*/\1$initialBalance/" "src/it/resources/template.conf"
echo "[-----INFO-----] initial-balance =$initialBalance"
sed -i -c "s/\(initial-mint-time *=*\).*/\1$initialMintTime/" "src/it/resources/template.conf"
echo "[-----INFO-----] initial-mint-time =$initialMintTime"
sed -i -c "s/\(signature *=*\).*/\1$signature/" "src/it/resources/template.conf"
echo "[-----INFO-----] signature =$signature"
sed -i -c "s/\(transactions *=*\).*/\1$transactions/" "src/it/resources/template.conf"
echo "[-----INFO-----] transactions =$transactions"

seed_index=0
while read line
do
    key=$(echo $line| cut -d '=' -f 1| tr -d ' '| tr -d '{')
    if [ "$key" = "known-peers" ]
    then
        known_peers=$(echo $line| cut -d '=' -f 2)
        sed -i -c "s/\(known-peers *=*\).*/\1$known_peers/" "src/it/resources/template.conf"
        echo "[-----INFO-----] known-peers =$known_peers"

    elif [ "$key" = "seed" ]
    then
        seed[seed_index]=$(echo $line| cut -d '=' -f 2)
        seed_index=$(( $seed_index + 1 ))

    elif [ "$key" = "miner" ]
    then
        mode="miner"
    elif [ "$key" = "peer" ]
    then
        mode="peer"
    elif [ "$key" = "quorum" ] && [ "$mode" = "miner" ]
    then
        miner_quorum=$(echo $line| cut -d '=' -f 2)
    elif [ "$key" = "quorum" ] && [ "$mode" = "peer" ]
    then
        peer_quorum=$(echo $line| cut -d '=' -f 2)
    fi

done < testnet_easy_start.conf

sbt clean
echo "[-----INFO-----] Old target cleaned!"
sbt -Dnetwork=testnet packageAll

echo "[-----INFO-----] Project compiled!"
sbt it:test
echo "[-----INFO-----] Docker file completed!"

echo "[-----INFO-----] Stopping and Removing old containers..."
docker stop $(docker ps -a -q)
docker rm $(docker ps -a -q)

echo "[-----INFO-----] Removing old images..."
docker rmi $(docker images 'peer_node*')
docker rmi $(docker images 'minter_node*')

echo "[-----INFO-----] Building peer node(s)!"

ip=19923
peer_quorum_index=$(( $peer_quorum - 1 ))

for i in $(seq 0 $peer_quorum_index)
do
    sed -i -c "s/\(seed *=*\).*/\1${seed[i]}/" "target/docker/1/template.conf"
    docker build -t peer_node_img${i} target/docker

    fip=$ip
    ip=$(( $ip - 1 ))
    sip=$ip
    ip=$(( $ip - 1 ))

    echo "[-----INFO-----] Starting peer node..."
    osascript -e 'tell app "Terminal" to do script "docker run --name peer_container'$i' -p '$fip':9923 -p '$sip':9922 peer_node_img'$i' -it sh"'
    echo "[-----INFO-----] Peer node started!"
done


echo "[-----INFO-----] Building minter node(s)!"

if [ -x minter_target ]; then
    	rm -rf minter_target
fi
mkdir minter_target
mkdir minter_target/docker

cp -a target/docker/. minter_target/docker

ip=9923
total=$((peer_quorum + miner_quorum - 1))

for i in $(seq $peer_quorum $total)
do
    sed -i -c "s/\(seed *=*\).*/\1${seed[i]}/" "minter_target/docker/1/template.conf"
    docker build -t minter_node_img${i}  minter_target/docker

    fip=$ip
    ip=$(( $ip - 1 ))
    sip=$ip
    ip=$(( $ip - 1 ))
    echo "[-----INFO-----] Starting minter node..."
    osascript -e 'tell app "Terminal" to do script "docker run --name minter_container'$i' -p '$fip':9923 -p '$sip':9922 minter_node_img'$i' -it sh"'
    echo "[-----INFO-----] Minter node started!"
done
