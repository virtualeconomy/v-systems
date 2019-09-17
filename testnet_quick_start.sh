#!/bin/bash

# Choose default/customized mode to create testnet
scriptModeOpt=""
requestTimes=0
while [ -z $scriptModeOpt ]
do
    read -p "Enter [d(default)/c(customized)]: " opt
    requestTimes=$(($requestTimes + 1))
    if [ $opt = "d" ] || [ $opt = "default" ];then
        scriptModeOpt="default"
        echo "[-----INFO-----] Started in Default mode to create a testnet"
    elif [ $opt = "c" ] || [ $opt = "customized" ];then
        scriptModeOpt="customized"
        echo "[-----INFO-----] Started in Customized mode to create a testnet"
    elif [ $requestTimes -ge 3 ]; then
        echo "[-----INFO-----] Shell script has been terminated!"
        break
    else
        continue
    fi
done

# Check to replace the existing v-systems directory
if [ -d "v-systems" ];then
    echo "\"v-systems\" already exists. Do you want to replace it?"
    replaceOpt=""
    requestTimes=0
    while [ -z $replaceOpt ]
    do
        read -p "Enter [y(yes)/n(no)]: " opt
        requestTimes=$(($requestTimes + 1))
        if [ $opt = "y" ] || [ $opt = "yes" ];then
            replaceOpt="true"
            rm -rf v-systems
            echo "[-----INFO-----] \"v-systems\" has already been removed"
        elif [ $opt = "n" ] || [ $opt = "no" ];then
            replaceOpt="false"
            echo "[-----INFO-----] Shell script has been terminated!"
        elif [ $requestTimes -ge 3 ]; then
            echo "[-----INFO-----] Shell script has been terminated!"
            break
        else
            continue
        fi
    done
fi

# Download "v-systems" to current directory
git clone https://github.com/virtualeconomy/v-systems.git
cd v-systems
echo "[-----INFO-----] V SYSTEMS project has cloned to the current directory"
git checkout isolatedNet

# No package compilation in Default mode
if [ $scriptModeOpt = "customized" ];then
    sbt clean
    echo "[-----INFO-----] Old target cleaned"

    # Package as target/vsys-all-0.1.1.jar
    sbt -Dnetwork=testnet packageAll
    echo "[-----INFO-----] Project compiled"

    sbt it:test
    echo "[-----INFO-----] Docker file completed"

    # Create genesis_settings.txt, or exit if failed to create the file
    touch genesis_settings.txt || exit

    # Compile the package and export the genesis block information to genesis_settings.txt
    java -cp "target/vsys-all-0.1.1.jar" vsys.utils.GenesisBlockGenerator > genesis_settings.txt
fi

# Get transactions, timestamp, blockTimestamp, averageBlockDelay, initialBalance, initialMintTime, signature from genesis_settings.txt
slot=0
transactions="["
transactionMode=false
while read line
do
    if [ $transactionMode = false ];then
        key=$(echo $line| cut -d ':' -f 1| tr -d ' ')
    else
        key=$(echo $line| cut -d '(' -f 1| tr -d ' ')
        if [ "$key" = "GenesisTransactionSettings" ];then
            transaction=$(echo $line| cut -d '(' -f 2)
            address=$(echo $transaction| cut -d ',' -f 1)
            amount=$(echo $transaction| cut -d ',' -f 2)
            # echo $address, $amount
            if [ $slot = 0 ];then
                transactions+="{recipient = $address, amount = $amount, slot-id = $slot}"
            else
                transactions+=",{recipient = $address, amount = $amount, slot-id = $slot}"
            fi
            slot=$(( $slot + 1 ))
        else
            transactionMode=false
        fi
    fi

    if [ "$key" = "timestamp" ];then
        timestamp=$(echo $line| cut -d ':' -f 2)
    elif [ "$key" = "blockTimestamp" ];then
        blockTimestamp=$(echo $line| cut -d ':' -f 2)
    elif [ "$key" = "averageBlockDelay" ];then
        averageBlockDelay=$(echo $line| cut -d ':' -f 2)
        averageBlockDelay=$(echo $averageBlockDelay| cut -d ' ' -f 1)s
    elif [ "$key" = "initialBalance" ];then
        initialBalance=$(echo $line| cut -d ':' -f 2)
    elif [ "$key" = "initialMintTime" ];then
        initialMintTime=$(echo $line| cut -d ':' -f 2)
    elif [ "$key" = "signature" ];then
        signature=$(echo $line| cut -d ':' -f 2)
        signature=${signature:6:88}
    elif [ "$key" = "transactions" ];then
        transactionMode=true
    fi
done < genesis_settings.txt

transactions+="]"

# Update src/it/resources/template.conf with above obtained parameters
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
sed -i -c "s/\(allow-contract-transaction-after-height *=*\).*/\11/" "src/it/resources/template.conf"
echo "[-----INFO-----] allow-contract-transaction-after-height =1"

# Get seed, miner quorum and peer quorum from testnet_quick_start.conf, put known-peers in src/it/resources/template.conf
seed_index=0
while read line
do
    key=$(echo $line| cut -d '=' -f 1| tr -d ' '| tr -d '{')
    if [ "$key" = "known-peers" ];then
        known_peers=$(echo $line| cut -d '=' -f 2)
        sed -i -c "s/\(known-peers *=*\).*/\1$known_peers/" "src/it/resources/template.conf"
        echo "[-----INFO-----] known-peers =$known_peers"

    elif [ "$key" = "seed" ];then
        seed[seed_index]=$(echo $line| cut -d '=' -f 2)
        seed_index=$(( $seed_index + 1 ))

    elif [ "$key" = "miner" ];then
        mode="miner"
    elif [ "$key" = "peer" ];then
        mode="peer"
    elif [ "$key" = "quorum" ] && [ "$mode" = "miner" ];then
#        miner_quorum=$(echo $line| cut -d '=' -f 2)
         miner_quorum=1
    elif [ "$key" = "quorum" ] && [ "$mode" = "peer" ];then
#        peer_quorum=$(echo $line| cut -d '=' -f 2)
         peer_quorum=0
    fi
done < testnet_quick_start.conf

mv src/it/resources/template.conf src/it/resources/template-new.conf

git checkout master
echo "[-----INFO-----] Check out to master branch"

rm src/it/resources/template.conf
mv src/it/resources/template-new.conf src/it/resources/template.conf

sbt clean
echo "[-----INFO-----] Old target cleaned"

sbt -Dnetwork=testnet packageAll
echo "[-----INFO-----] Project compiled"

sbt it:test
echo "[-----INFO-----] Docker file completed"

echo "[-----INFO-----] Stopping and Removing old containers..."
docker stop $(docker ps -a -q)
docker rm $(docker ps -a -q)

echo "[-----INFO-----] Removing old images..."
docker rmi $(docker images 'peer_node*')
docker rmi $(docker images 'minter_node*')

echo "[-----INFO-----] Building peer node(s)"
ip=19923
peer_quorum_index=$(( $peer_quorum - 1 ))

for i in $(seq 0 $peer_quorum_index)
do
    sed -i -c "s/\(seed *=*\).*/\1${seed[i]}/" "target/docker/1/template.conf"
    docker build -t peer_node_img${i} target/docker

    fip=$ip
    ip=$(( $ip - 1 ))
    sip=$ip
    ip=$(( $ip + 10001 ))

    echo "[-----INFO-----] Starting peer node..."
    osascript -e 'tell app "Terminal" to do script "docker run --name peer_container'$i' -p '$fip':9923 -p '$sip':9922 peer_node_img'$i' -it sh"'
    echo "[-----INFO-----] Peer node started"
done

echo "[-----INFO-----] Building minter node(s)"

if [ -x minter_target ]; then
    rm -rf minter_target
fi
mkdir minter_target
mkdir minter_target/docker

cp -a target/docker/. minter_target/docker

ip=9923
total=$((peer_quorum + miner_quorum))
miner_quorum_index=$(( $total - 1 ))

if [ $total -gt $peer_quorum ];then
    for i in $(seq $peer_quorum $miner_quorum_index)
    do
        sed -i -c "s/\(seed *=*\).*/\1${seed[i]}/" "minter_target/docker/1/template.conf"
        docker build -t minter_node_img${i}  minter_target/docker

        fip=$ip
        ip=$(( $ip - 1 ))
        sip=$ip
        ip=$(( $ip - 1 ))
        echo "[-----INFO-----] Starting minter node..."
        osascript -e 'tell app "Terminal" to do script "docker run --name minter_container'$i' -p '$fip':9923 -p '$sip':9922 minter_node_img'$i' -it sh"'
        echo "[-----INFO-----] Minter node started"
    done
fi
