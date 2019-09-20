#!/bin/bash

## Configured Parameters in Default Mode
PEER_IMG_REPO="julianip/peer_node"
MINER_IMG_REPO="julianip/minter_node"

## Configured Parameters in Customized Mode
PEER_NUM=1
MINER_NUM=1
INITIAL_BALANCE=1000000000000000000

## Choose default/customized mode to create testnet
scriptModeOpt=""
requestTimes=0
while [ -z $scriptModeOpt ]
do
    read -p "Enter [default(d)/customized(c)]: " opt
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

total=$((PEER_NUM + MINER_NUM))
interval=$((60/$total))

if [ $scriptModeOpt = "default" ];then
    echo "[-----INFO-----] Stopping and Removing old containers..."
    docker stop $(docker ps -a -q)
    docker rm $(docker ps -a -q)

    echo "[-----INFO-----] Removing old images..."
    customized_peer_image_num=$(docker images --format "{{.ID}}" 'peer_node*'|wc -l| tr -d ' ')
    customized_miner_image_num=$(docker images --format "{{.ID}}" 'minter_node*'|wc -l| tr -d ' ')
    if [ $customized_peer_image_num -gt 0 ];then
        docker rmi $(docker images --format "{{.ID}}" 'peer_node*')
    fi
    if [ $customized_miner_image_num -gt 0 ];then
        docker rmi $(docker images --format "{{.ID}}" 'minter_node*')
    fi

    default_peer_image_num=$(docker images --format "{{.ID}}" $PEER_IMG_REPO|wc -l| tr -d ' ')
    default_miner_image_num=$(docker images --format "{{.ID}}" $MINER_IMG_REPO|wc -l| tr -d ' ')

    if [ $default_peer_image_num -eq 0 ];then
        docker pull $PEER_IMG_REPO
    fi
    if [ $default_miner_image_num -eq 0 ];then
        docker pull $MINER_IMG_REPO
    fi

    echo "[-----INFO-----] Starting peer node..."
    osascript -e 'tell app "Terminal" to do script "docker run --name peer_container -p 19923:9923 -p 19922:9922 '$PEER_IMG_REPO' -it sh"'
    echo "[-----INFO-----] Peer node started"

    echo "[-----INFO-----] Starting minter node..."
    osascript -e 'tell app "Terminal" to do script "docker run --name minter_container -p 9923:9923 -p 9922:9922 '$MINER_IMG_REPO' -it sh"'
    echo "[-----INFO-----] Minter node started"

else
    ## Prepare testnet_quick_start.conf
    # Check to download the v-wallet-generator directory
    if [ ! -d "v-wallet-generator" ];then
        # Download "v-systems" to current directory
        git clone https://github.com/virtualeconomy/v-wallet-generator
        echo "[-----INFO-----] V Wallet Generator project has cloned to the current directory"

        # create walletgenerator_v0.1.0.jar under target directory
        mkdir target
        curl -O https://github.com/virtualeconomy/v-wallet-generator/releases/walletgenerator_v0.1.0.jar
        mv walletgenerator_v0.1.0.jar /target
    fi

    cd v-wallet-generator

    ## Create testnet_quick_start.conf, or exit if failed to create the file
    touch testnet_quick_start.conf || exit

    echo "[-----INFO-----] Initializing $total Wallet Address(es)..."
    echo vsys { > testnet_quick_start.conf
    echo $'\t'network { >> testnet_quick_start.conf
    echo $'\t'$'\t'known-peers = [\"172.17.0.2:9923\"] >> testnet_quick_start.conf
    echo $'\t'} >> testnet_quick_start.conf
    echo $'\t'miner { >> testnet_quick_start.conf
    echo $'\t'$'\t'quorum = 2 >> testnet_quick_start.conf
    echo $'\t'} >> testnet_quick_start.conf
    echo $'\t'peer { >> testnet_quick_start.conf
    echo $'\t'$'\t'quorum = 3 >> testnet_quick_start.conf
    echo $'\t'} >> testnet_quick_start.conf
    echo $'\t'wallet { >> testnet_quick_start.conf
    echo $'\t'$'\t'initial-balance = $INITIAL_BALANCE >> testnet_quick_start.conf
    echo $'\t'$'\t'slots { >> testnet_quick_start.conf

    ## Calculate the balance_distribution of the addresses
    declare -a balance_distribution_arr
    max=$(bc -l <<< 1/$total)
    sum=1
    for i in $(seq 0 $(($total-2)))
    do
        val=$(echo $(jot -r 1 0.01 $max))
        try_sum=$(bc -l <<< $sum-$val)
        while (( $(echo "$val < 0" |bc -l) )) || (( $(echo "$try_sum < 0" |bc -l) ))
        do
            val=$(echo $(jot -r 1 0.01 $max))
            try_sum=$(bc -l <<< $sum-$val)
        done

        sum=$(bc -l <<< $sum-$val)
        balance_distribution_arr[i]=$val
    done

    balance_distribution_arr[$(($total-1))]=0$sum

    ## Continue Update testnet_quick_start.conf
    for i in $(seq 0 $(($total-1)));
    do
        full_result=$(java -cp "target/walletgenerator_v0.1.0.jar" WalletGenerator --testnet)
        slot_index=$(($i*$interval))
        echo $'\t'$'\t'$'\t'slot$slot_index { >> testnet_quick_start.conf

        seed=$(echo $(echo $full_result| cut -d ':' -f 2)| cut -d '-' -f 1)
        seed=${seed:0:$((${#seed}-1))}
        echo $'\t'$'\t'$'\t'$'\t'seed = \"$seed\" >> testnet_quick_start.conf

        address=$(echo $(echo $full_result| cut -d ':' -f 4)| cut -d ' ' -f 1)
        echo $'\t'$'\t'$'\t'$'\t'address = $address >> testnet_quick_start.conf

        public_key=$(echo $(echo $full_result| cut -d ':' -f 5)| cut -d ' ' -f 1)
        echo $'\t'$'\t'$'\t'$'\t'public_key = $public_key >> testnet_quick_start.conf

        private_key=$(echo $(echo $full_result| cut -d ':' -f 6)| cut -d ' ' -f 1)
        echo $'\t'$'\t'$'\t'$'\t'private_key = $private_key >> testnet_quick_start.conf

        account_seed=$(echo $(echo $full_result| cut -d ':' -f 7)| cut -d ' ' -f 1)
        echo $'\t'$'\t'$'\t'$'\t'account_seed = $account_seed >> testnet_quick_start.conf

        balance_distribution=${balance_distribution_arr[$i]}
        echo $'\t'$'\t'$'\t'$'\t'balance_distribution = $balance_distribution >> testnet_quick_start.conf

        echo $'\t'$'\t'$'\t'} >> testnet_quick_start.conf

    done
    echo $'\t'$'\t'} >> testnet_quick_start.conf
    echo $'\t'} >> testnet_quick_start.conf
    echo } >> testnet_quick_start.conf

    sed -i -c "6s/\(quorum *=*\).*/\1 $MINER_NUM/" "testnet_quick_start.conf"
    sed -i -c "9s/\(quorum *=*\).*/\1 $PEER_NUM/" "testnet_quick_start.conf"

    ## Check to replace the existing v-systems directory
    cd ..
    if [ -d "v-systems" ];then
        echo "\"v-systems\" already exists. Do you want to replace it?"
        replaceOpt=""
        requestTimes=0
        while [ -z $replaceOpt ]
        do
            read -p "Enter [yes(y)/no(n)]: " opt
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

    ## Download "v-systems" to current directory
    git clone https://github.com/virtualeconomy/v-systems
    echo "[-----INFO-----] V SYSTEMS project has cloned to the current directory"
    cd v-systems
    git checkout isolatedNet
    mv ../v-wallet-generator/testnet_quick_start.conf .

    ## Prepare genesis_settings.txt
    if [ -f "vsys-all-0.1.1.jar" ];then
        # Create genesis_settings.txt, or exit if failed to create the file
        touch genesis_settings.txt || exit

        # Compile the package and export the genesis block information to genesis_settings.txt
        java -cp "vsys-all-0.1.1.jar" vsys.utils.GenesisBlockGenerator > genesis_settings.txt
    else
        sbt clean
        echo "[-----INFO-----] Old target cleaned"

        # Package as target/vsys-all-0.1.1.jar
        sbt -Dnetwork=testnet packageAll
        echo "[-----INFO-----] Project compiled"

        # Create genesis_settings.txt, or exit if failed to create the file
        touch genesis_settings.txt || exit

        # Compile the package and export the genesis block information to genesis_settings.txt
        java -cp "target/vsys-all-0.1.1.jar" vsys.utils.GenesisBlockGenerator > genesis_settings.txt
    fi

    ## Get transactions, timestamp, blockTimestamp, averageBlockDelay, initialBalance, initialMintTime, signature from genesis_settings.txt
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
                slot=$(( $slot + $interval ))
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

    ## Update src/it/resources/template.conf with above obtained parameters
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

    ## Get seed, miner quorum and peer quorum from testnet_quick_start.conf, put known-peers in src/it/resources/template.conf
    seed_index=0
    declare -a seed_arr
    while read line
    do
        key=$(echo $line| cut -d '=' -f 1| tr -d ' '| tr -d '{')
        if [ "$key" = "known-peers" ];then
            known_peers=$(echo $line| cut -d '=' -f 2)
            sed -i -c "s/\(known-peers *=*\).*/\1$known_peers/" "src/it/resources/template.conf"
            echo "[-----INFO-----] known-peers =$known_peers"

        elif [ "$key" = "seed" ];then
            seed_arr[seed_index]=$(echo $line| cut -d '=' -f 2)
            seed_index=$(( $seed_index + 1 ))
        fi
    done < testnet_quick_start.conf

    mv src/it/resources/template.conf src/it/resources/template-new.conf

    git checkout master
    echo "[-----INFO-----] Check out to master branch"

    mv src/it/resources/template-new.conf src/it/resources/template.conf

    sbt clean
    echo "[-----INFO-----] Old target cleaned"

    sbt -Dnetwork=testnet packageAll
    echo "[-----INFO-----] Project compiled"

    vsys_image_num=$(docker images --format "{{.ID}}" 'systems.v/vsys'|wc -l| tr -d ' ')
    if [ $vsys_image_num -gt 0 ];then
        docker rmi $(docker images --format "{{.ID}}" 'systems.v/vsys')
    fi
    sbt it:test
    echo "[-----INFO-----] Docker file completed"

    echo "[-----INFO-----] Stopping and Removing old containers..."
    minter_container_num=$(docker container ps -a -f name=minter_container* --format "{{.ID}}"|wc -l| tr -d ' ')
    peer_container_num=$(docker container ps -a -f name=peer_container* --format "{{.ID}}"|wc -l| tr -d ' ')
    if [ $minter_container_num -gt 0 ];then
        docker stop $(docker container ps -a -f name=minter_container* --format "{{.ID}}")
        docker rm $(docker container ps -a -f name=minter_container* --format "{{.ID}}")
    fi

    if [ $peer_container_num -gt 0 ];then
        docker stop $(docker container ps -a -f name=peer_container* --format "{{.ID}}")
        docker rm $(docker container ps -a -f name=peer_container* --format "{{.ID}}")
    fi

    echo "[-----INFO-----] Removing old images..."
    customized_peer_image_num=$(docker images --format "{{.ID}}" 'peer_node*'|wc -l| tr -d ' ')
    customized_miner_image_num=$(docker images --format "{{.ID}}" 'minter_node*'|wc -l| tr -d ' ')
    if [ $customized_peer_image_num -gt 0 ];then
        docker rmi $(docker images --format "{{.ID}}" 'peer_node*')
    fi

    if [ $customized_miner_image_num -gt 0 ];then
        docker rmi $(docker images --format "{{.ID}}" 'minter_node*')
    fi

    default_peer_image_num=$(docker images --format "{{.ID}}" $PEER_IMG_REPO|wc -l| tr -d ' ')
    default_miner_image_num=$(docker images --format "{{.ID}}" $MINER_IMG_REPO|wc -l| tr -d ' ')
    if [ $default_peer_image_num -gt 0 ];then
        docker rmi $(docker images --format "{{.ID}}" $PEER_IMG_REPO)
    fi
    if [ $default_miner_image_num -gt 0 ];then
        docker rmi $(docker images --format "{{.ID}}" $MINER_IMG_REPO)
    fi

    echo "[-----INFO-----] Building peer node(s)"
    ip=19923
    peer_quorum_index=$(( $PEER_NUM - 1 ))

    if [ $PEER_NUM -gt 0 ];then
        sed -i -c "37s/\(enable *=*\).*/\1 no/" "target/docker/1/template.conf"
        for i in $(seq 0 $peer_quorum_index)
        do
            sed -i -c "s/\(seed *=*\).*/\1${seed_arr[i]}/" "target/docker/1/template.conf"
            docker build -t peer_node_img${i} target/docker

            fip=$ip
            ip=$(( $ip - 1 ))
            sip=$ip
            ip=$(( $ip + 10001 ))

            echo "[-----INFO-----] Starting peer node..."
            osascript -e 'tell app "Terminal" to do script "docker run --name peer_container'$i' -p '$fip':9923 -p '$sip':9922 peer_node_img'$i' -it sh"'
            echo "[-----INFO-----] Peer node started"
        done
    fi

    echo "[-----INFO-----] Building minter node(s)"

    if [ -x minter_target ]; then
        rm -rf minter_target
    fi
    mkdir minter_target
    mkdir minter_target/docker

    cp -a target/docker/. minter_target/docker

    ip=9923
    miner_quorum_index=$(( $total - 1 ))

    if [ $total -gt $PEER_NUM ];then
        sed -i -c "37s/\(enable *=*\).*/\1 yes/" "minter_target/docker/1/template.conf"
        for i in $(seq $PEER_NUM $miner_quorum_index)
        do
            sed -i -c "s/\(seed *=*\).*/\1${seed_arr[i]}/" "minter_target/docker/1/template.conf"
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
fi
