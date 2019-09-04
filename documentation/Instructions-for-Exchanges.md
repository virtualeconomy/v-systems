# Instructions for Exchanges
The purpose of this document is to describe how to interface your exchange with the V Systems (VSYS) blockchain step-by-step.

交易所对接指南(中文版)点击[这里](https://github.com/virtualeconomy/v-systems/wiki/%E4%BA%A4%E6%98%93%E6%89%80%E5%AF%B9%E6%8E%A5%E6%8C%87%E5%8D%97%28%E4%B8%AD%E6%96%87%29)

# Preparation

## Hardware

In current stage, the standard hardware requirement is VPS with 2 CPU, 16GB of RAM and 512GB HDD.

The recommend requirement is Amazon Web Services (AWS) i3 large.

## Software

### Operating System

All Java 1.8 and Python runnable operating system are supported (including Ubuntu, CentOS, MacOS, Windows etc.).

The recommended operating system is **Ubuntu 16.04 LTS** (or above).

In this instruction, we'll take Ubuntu 16.04 for instance.

### Service installation

First of all, update the repository

```shell
$ sudo apt-get update
```

Install Java in your machine

```shell
$ sudo apt-get install openjdk-8-jdk
```

Check Java version (remove the old version Java if needed).

```shell
$ java -version
openjdk version "1.8.0_181"
OpenJDK Runtime Environment (build 1.8.0_181-8u181-b13-0ubuntu0.16.04.1-b13)
OpenJDK 64-Bit Server VM (build 25.181-b13, mixed mode)
```

Install Scala Build Tool (SBT) if you want to compile v systems node project

```shell
$ echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
$ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
$ sudo apt-get update
$ sudo apt-get install sbt
```

Install Unzip and Git if your machine do not have this tool.

```shell
$ sudo apt-get install unzip
$ sudo apt-get install git-core
```

## Start V Systems full node

### Step 1: Prepare

There is two ways to prepare to node program file. Choose the solution as you want.

#### Download source code and Compile (Method 1)

Download source code from GitHub.

```shell
$ git clone https://github.com/virtualeconomy/v-systems.git
$ cd vsys
```

Then compile by SBT. If you want to compile TestNet V Systems node,

```shell
# Compile TestNet V Systems node
$ sbt -Dnetwork=testnet packageAll
```

Or otherwise, if you want to compile MainNet V Systems node,

```shell
# Compile MainNet V Systems node
$ sbt packageAll
```

The compiled JAR file will be located at **target/vsys-all-[version].jar**. Copy to your own path as you want. For example,

```shell
$ mkdir ../vsys-node
$ cp target/vsys-all-*.jar ../vsys-node/v-systems.jar
$ cd ../vsys-node
```


#### Download compiled file (Method 2)

If you do not want to compile souce code, you could download the lastest JAR from https://github.com/virtualeconomy/v-systems/releases as well.

Choose **v-systems-[version].jar** and save to your own path as you want.

### Step 2: Configuration

Set your configuration file.

```
# V Systems node settings
vsys {
  # Path Settings
  directory = <system data & wallet data folder path>
  data-directory = <block data folder path>

  # Application logging level. Could be DEBUG | INFO | WARN | ERROR. Default value is INFO.
  logging-level = INFO
  # P2P Network settings
  network {
    known-peers = ["<peer ip>:<peer port>"]
    black-list-residence-time = 30s
    peers-broadcast-interval = 5s
    connection-timeout = 30s
    # Network address to bind to
    bind-address = "0.0.0.0"
    # Node name to send during handshake. Comment this string out to set random node name.
    # node-name = "My MAINNET node"
    # String with IP address and port to send as external address during handshake. Could be set automatically if uPnP is enabled.
    declared-address = "localhost:9921"
  }
  # Wallet settings
  wallet {
    # Password to protect wallet file
    password = ""
    # Wallet seed as string
    # seed = ""
  }
  # Blockchain settings
  blockchain.type = TESTNET   # Should be TESTNET or MAINNET
  # Matcher settings
  matcher.enable = no
  # Minter settings
  miner {
    enable = yes
    offline = no
    quorum = 1
    generation-delay = 1s
    interval-after-last-block-then-generation-is-allowed = 120h
    tf-like-scheduling = no
    # Left to empty as default to minter address
    reward-address = ""
  }
  # Node's REST API settings
  rest-api {
    # Enable/disable node's REST API
    enable = yes
    # Network address to bind to
    bind-address = "0.0.0.0"
    # Hash of API key string
    api-key-hash = "Fo8fR7J1gB3k2WoaE6gYKMwgWfoh9EtZtXAMBxYYCJWG"
  }
  checkpoints.public-key = "A9MX22tXpNdTTx5wBf3CunZz299c1nnca6kH3MzL312L"
}
```
#### Some key points of configuration
* The **directory** and **data-directory** should be set to your own path. For **data-directory**, we suggest you mount a large disk and set the directory to this disk.

* It is better to choose more than 3 peers to fill the **known-peers** field. You could check known-peers via default config file ([Testnet Config](https://github.com/virtualeconomy/v-systems/blob/master/vsys-testnet.conf), [Mainnet Config](https://github.com/virtualeconomy/v-systems/blob/master/vsys-mainnet.conf)). Some known-peers for current reference:

	```
	# For TestNet
	known-peers = ["54.193.47.112:9923","13.57.25.133:9923","18.218.106.1:9923","3.17.78.253:9923","34.222.191.174:9923"]
	# For MainNet (contact us to get more known peers)
	known-peers = ["13.55.174.115:9921","52.30.23.41:9921","13.113.98.91:9921","3.121.94.10:9921"]
	```

* The **blockchain.type** should be filled with TESTNET or MAINNET.

* For security reason, it is better to set you own **api-key-hash**. You could check the hash by this command

	```
	curl -X POST -d '<input your api key>' 'https://test.v.systems/api/utils/hash/secure'
	```

* Finnaly, we save the file, for example name it as "vsys.conf".

### Step 3: Run

```shell
$ screen -S vsys-node
$ sudo java -jar v-systems*.jar vsys.conf
```

Detach your screen by `Ctrl + A + D` .

To inspect the screen

```shell
$ screen -x vsys-node
```

## Full Node API Operation

Security warning: Every full node provides RESTful API for interaction with chain. The RESTful API service will use port 9922. For security reason, we suggest the exchange modify firewall rule and **not open 9922 in public network**, only for internal network using. To make communication among the nodes easy and smooth, please keep port 9921 (mainnet) and 9923 (testnet) opening in public.

You could use the following method to call APIs.


### Method 1: Use SDK integration

We strongly suggest you use SDK to do integration. Currently, we provide the following version of SDK:

Python version SDK [pyvsystems](https://github.com/virtualeconomy/pyvsystems). The pyvsystems specification is [here](https://github.com/virtualeconomy/pyvsystems/wiki/PYVSYSTEMS-User-Guide-Specification-%28English%29).

JavaScript version SDK [js-v-sdk](https://github.com/virtualeconomy/js-v-sdk)

Java version SDK [java-v-sdk](https://github.com/virtualeconomy/java-v-sdk)

C# version SDK  [cs-v-sdk](https://github.com/virtualeconomy/cs-v-sdk)

Golang version SDK [go-v-sdk](https://github.com/virtualeconomy/go-v-sdk)

### Method 2: Use cURL to call API

You could open ```http://<full node ip>:9922``` in browser to find all APIs you can use.

#### Step 1: Prepare

Install curl for post http request if you do not have curl.

```shell
$ sudo apt-get install curl
```

To test connection, you could use

```shell
$ curl -X GET 'http://<full node ip>:9922/blocks/height'
```

If success, you will get more or less like the following response:

```
{
  "height": 2400326
}
```

#### Step 2: Create wallet

Use HTTP POST to call /addresses API.

```shell
$ curl -X POST --header 'Accept: application/json' --header 'api_key: <full node api key>' 'http://<full node ip>:9922/addresses'
```

If wallet is created, you will get address of the wallet in response:

```
{
  "address": "AUBBmrf5cuBf9XrSaX98mxWmcNBwULqQhQK"
}
```

The created wallets will be stored in ```<block data folder path>/wallet/wallet.dat```. Remember to backup the file in schedule.
And you can recover a wallet from seed as well. To find the seed of wallet, use HTTP GET to call /addresses/seed/{address} with node API key. For example,

```shell
$ curl -X GET --header 'Accept: application/json' --header 'api_key:  <full node api key>' 'http://<full node ip>:9922/addresses/seed/AU9KCwJm6mG9YxSb3LdjVi6LDwRGey1knfy'
```

Response:

```
{
  "address": "AU9KCwJm6mG9YxSb3LdjVi6LDwRGey1knfy",
  "seed": "GY7T8WpppuficZJs9CnEuntLkk4vXw7qkZ1SMtZ3qAas"
}
```

Once you known the seed, you can recover the wallet by [wallet-generator](https://github.com/virtualeconomy/v-wallet-generator)

#### Step 3: Check address and balance

Use HTTP GET calling /addresses API to get all addresses:

```shell
$ curl -X GET 'http://<full node ip>:9922/addresses'
```

Response:

```
[
  "ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV",
  "ATys7iafCN4xHz9bJyKm4JNfKpk9f1uBBXT",
  "AU1TFgjs3g1NMkXW5CGTGj96t8qijs6ScrP",
  "ATubqbssJKtPfyebkmct4jv9YSue8xrhMLa",
  "AU9KCwJm6mG9YxSb3LdjVi6LDwRGey1knfy",
  "ATwMEAGfNhbRCRSApro8HWG2L65HMMa42KP",
  "AU4rMtj3zEJesVQ94Az8ajncNMgtK6uzeHB",
  "ATtdkLaHDPZQx3LsUfrKisRcrMkvfg18LGa",
  "ATxa6h87rBrNYKDCkagageiPzTMFgcGmefA",
  "AU8R9ri7eG968zuJuLQVLMiUzRNXvQwNPwE"
]
```

As more and more wallets created, it is better to query the wallet address with number limit. You can use HTTP GET /addresses/seq/{from}/{to} to get any range of addresses. For example:

```shell
$ curl -X GET 'http://<full node ip>:19922/addresses/seq/5/10'
```

Response:

```
[
  "ATwMEAGfNhbRCRSApro8HWG2L65HMMa42KP",
  "AU4rMtj3zEJesVQ94Az8ajncNMgtK6uzeHB",
  "ATtdkLaHDPZQx3LsUfrKisRcrMkvfg18LGa",
  "ATxa6h87rBrNYKDCkagageiPzTMFgcGmefA",
  "AU8R9ri7eG968zuJuLQVLMiUzRNXvQwNPwE"
]
```

To check balance of address, use HTTP GET to call /addresses/balance/details/{address}

```shell
$ curl -X GET 'http://<full node ip>:9922/addresses/balance/details/AU8R9ri7eG968zuJuLQVLMiUzRNXvQwNPwE'
```

Response:

```
{
	'address': 'AU8R9ri7eG968zuJuLQVLMiUzRNXvQwNPwE',
	'regular': 109010000000, 		# regular balance
	'available': 108910000000,  		# available balance (regular - leased out)
	'effective': 108910000000,  		# effective balance (regular - leases out + leased in)
	'mintingAverage': 108909964800,  	# for minter used
	'height': 643936
}
```

The ```available``` balance is the actual balance which wallet currently could use (100000000 = 1 VSYS).

#### Step 4: Make payment transaction

Use HTTP POST to call /vsys/payment API.

```shell
$ curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' --header 'api_key: <full node api key>' -d '{ \ 
   "amount": 100000000, \ 
   "fee": 10000000, \ 
   "feeScale": 100, \ 
   "sender": "ATtRykARbyJS1RwNsA6Rn1Um3S7FuVSovHK", \ 
   "attachment": "", \ 
   "recipient": "ATt6P4vSpBvBTHdV5V9PJEHMFp4msJ1fkkX" \ 
 }' 'http://<full node ip>:9922/vsys/payment'
```

In request,

```amount``` is the payment amount. 100000000 = 1 VSYS.

```fee``` is the transaction fee. The mininum transaction fee is 10000000(0.1 VSYS).

```feeScale ``` currently is a fixed value. It should be 100.

```sender ``` is the sender wallet address.

```recipient ``` is the recipient wallet address.

```attachment ``` could be any text (max 140 characters) with base58 encoding.


Response:

```
{
  "type": 2,
  "id": "EoNQyNouEKg8pDcEEPY2dJL9FMQx61YFk1Sn5EJN8H7K",
  "fee": 10000000,
  "timestamp": 1544083814291691000,
  "proofs": [
    {
      "proofType": "Curve25519",
      "publicKey": "3orvgyRKf45FRyiCkcA3CzAGDvyEpBpXZzYGEGZnpZK5",
      "signature": "t1X2zmw5a2b9iaLgtsHyHKgEmKo6GCFuMFQsZNqj8ZkzpVbRKhWUttqUDfcjzcn5w7VgVVvf8cetr1mh2d2xypQ"
    }
  ],
  "recipient": "ATt6P4vSpBvBTHdV5V9PJEHMFp4msJ1fkkX",
  "feeScale": 100,
  "amount": 100000000,
  "attachment": ""
}
```

#### Step 5: Check payment transaction

Use HTTP GET to call /transactions/address/{address}/limit/{limit} API. For example,

```shell
$ curl -X GET --header 'Accept: application/json' 'http://<full node ip>:9922/transactions/address/ATt6P4vSpBvBTHdV5V9PJEHMFp4msJ1fkkX/limit/5'
```

Response:

```
[
  [
    {
      "type": 2,
      "id": "EoNQyNouEKg8pDcEEPY2dJL9FMQx61YFk1Sn5EJN8H7K",
      "fee": 10000000,
      "timestamp": 1544083814291691000,
      "proofs": [
        {
          "proofType": "Curve25519",
          "publicKey": "3orvgyRKf45FRyiCkcA3CzAGDvyEpBpXZzYGEGZnpZK5",
          "signature": "t1X2zmw5a2b9iaLgtsHyHKgEmKo6GCFuMFQsZNqj8ZkzpVbRKhWUttqUDfcjzcn5w7VgVVvf8cetr1mh2d2xypQ"
        }
      ],
      "recipient": "ATt6P4vSpBvBTHdV5V9PJEHMFp4msJ1fkkX",
      "feeScale": 100,
      "amount": 100000000,
      "attachment": "",
      "status": "Success",
      "feeCharged": 10000000
    },
    {
      "type": 4,
      "id": "FiMiErppddPfFCmehu1ziKNTqyzBFsLRj6gh9y45JKKD",
      "fee": 10000000,
      "timestamp": 1543569020372515800,
      "proofs": [
        {
          "proofType": "Curve25519",
          "publicKey": "B2Khd89jtnpuzGdnyGRcnKycZMBCo6PsotFcWWi1wMDV",
          "signature": "2hpsVXZVs2Wmg5ixD8PqvMJoC3CAqgTqvapYkuFAxbLvoyXRu45q9HXZQyqCzHeiHocGFM8phPkmDuM566Xu59em"
        }
      ],
      "feeScale": 100,
      "leaseId": "D8mGb2YSGyKr5Q3WATnpQP8JvyDdteXwieo5khwsTEyY",
      "status": "Success",
      "feeCharged": 10000000,
      "lease": {
        "type": 3,
        "id": "D8mGb2YSGyKr5Q3WATnpQP8JvyDdteXwieo5khwsTEyY",
        "fee": 10000000,
        "timestamp": 1543569009108564000,
        "proofs": [
          {
            "proofType": "Curve25519",
            "publicKey": "B2Khd89jtnpuzGdnyGRcnKycZMBCo6PsotFcWWi1wMDV",
            "signature": "8TDUgnkNbrPL6VMLFzDnhZvABfRqXitFX46mmvpohsdeRHKaNtWCs5C7m6avaUH2NjiFS7jGFov1CY5s3W8Zc5V"
          }
        ],
        "amount": 100000000,
        "recipient": "AU6GsBinGPqW8zUuvmjgwpBNLfyyTU3p83Q",
        "feeScale": 100
      }
    },
    {
      "type": 3,
      "id": "D8mGb2YSGyKr5Q3WATnpQP8JvyDdteXwieo5khwsTEyY",
      "fee": 10000000,
      "timestamp": 1543569009108564000,
      "proofs": [
        {
          "proofType": "Curve25519",
          "publicKey": "B2Khd89jtnpuzGdnyGRcnKycZMBCo6PsotFcWWi1wMDV",
          "signature": "8TDUgnkNbrPL6VMLFzDnhZvABfRqXitFX46mmvpohsdeRHKaNtWCs5C7m6avaUH2NjiFS7jGFov1CY5s3W8Zc5V"
        }
      ],
      "amount": 100000000,
      "recipient": "AU6GsBinGPqW8zUuvmjgwpBNLfyyTU3p83Q",
      "feeScale": 100,
      "status": "Success",
      "feeCharged": 10000000
    },
    {
      "type": 2,
      "id": "He17g3JXtbXgMiWCTGwnNMPfvfFH5tvyJfYZ7BiWGBZK",
      "fee": 10000000,
      "timestamp": 1543568995612184000,
      "proofs": [
        {
          "proofType": "Curve25519",
          "publicKey": "CbUPwcCJaMqYSjZGXy4LrkTfV2ncP27Chqyd2QKXfJxn",
          "signature": "24fNpVr8qjrKuDNd8JSeZgkSa4BuS44kxupiAYPHxCbbPMBs2DyT7VDnqAWsJkYZxXWadqiQs7HFxW9uVULrGAtt"
        }
      ],
      "recipient": "ATt6P4vSpBvBTHdV5V9PJEHMFp4msJ1fkkX",
      "feeScale": 100,
      "amount": 100000000,
      "attachment": "",
      "status": "Success",
      "feeCharged": 10000000
    },
    {
      "type": 2,
      "id": "2FVTJUpUJAhZJWkVYHHCG4nRXkYqwQcKsGEK8uwx3A58",
      "fee": 10000000,
      "timestamp": 1543568982176328000,
      "proofs": [
        {
          "proofType": "Curve25519",
          "publicKey": "B2Khd89jtnpuzGdnyGRcnKycZMBCo6PsotFcWWi1wMDV",
          "signature": "3DShPFQLidR1nbTKDrrhpp6SZdiL1hKtjYaGANzGuaWqjpGggPgtrCzw5XYXXktt2sFgWnmFVfTf4gNkmNPNyS2v"
        }
      ],
      "recipient": "AU6GsBinGPqW8zUuvmjgwpBNLfyyTU3p83Q",
      "feeScale": 100,
      "amount": 100000000,
      "attachment": "",
      "status": "Success",
      "feeCharged": 10000000
    }
  ]
]
```

Some common id of transaction types:

```
2 = Payment transaction
3 = Lease transaction
4 = Cancel lease transaction
5 = Minting transaction
```

#### Check transaction by ID

Use HTTP GET to call /transactions/info/{id} API. For example,

 ```shell
$ curl -X GET 'http://<node ip>:9922/transactions/info/EhmLJA5H5LG8a69eQEpKtbPZf8KGMCSH5w4MPDMoaGR3'
```

Response:

 ```
{
  "type": 2,
  "id": "EhmLJA5H5LG8a69eQEpKtbPZf8KGMCSH5w4MPDMoaGR3",
  "fee": 10000000,
  "timestamp": 1562339456608000000,
  "proofs": [
    {
      "proofType": "Curve25519",
      "publicKey": "EaxkrqBySftSD7M9WJiBKxLPjugtjUqDCJK3Lf3aTq1E",
      "signature": "4zxLLLpBmERW7zwTTyRamrQDgeQPSe2gwFwsUKoVtBvsPjz73n2fHFLBxAyYtJop3yrKs9LFiirNZ5VUDahD4ao7"
    }
  ],
  "recipient": "AU83FKKzTYCue5ZQPweCzJ68dQE4HtdMv5U",
  "feeScale": 100,
  "amount": 50000000,
  "attachment": "",
  "status": "Success",
  "feeCharged": 10000000,
  "height": 5414065
}
```

PS: If a transaction if packaged into a block from Unconfirmed transaction pool, this API will return the **height** of transaction. We can confirm a transaction if node block height is 31 blocks higher than the height  of transaction. 


### Cold Wallet Payment Signature

To make sure asset is safty, the exchange collects coins from hot wallet and stores these coins in cold wallet. When user widthdraw coins, the exchange transfer out to user from cold wallet. To transfer out the coins from cold wallet, the key point is generating the payment signature. The steps can refer the `send_payment(...)` method of [account.py](https://github.com/virtualeconomy/pyvsystems/blob/master/account.py) in [pyvsystems](https://github.com/virtualeconomy/pyvsystems).

And you can write your own program to generate payment signature as well. 
For example, if you want to send a payment which JSON format like this:

```
{
  "amount": 1000000000,
  "fee": 10000000,
  "feeScale": 100,
  "timestamp": 1547722056762119200,
  "recipient": "AU6GsBinGPqW8zUuvmjgwpBNLfyyTU3p83Q",
  "senderPublicKey": "B2Khd89jtnpuzGdnyGRcnKycZMBCo6PsotFcWWi1wMDV",
  "attachment": "HXRC"
}
```

Translate these fields into bytes:

```
type_id: 02
timestamp: 15 7a 9d 02 ac 57 d4 20
amount: 00 00 00 00 3b 9a ca 00
tx_fee: 00 00 00 00 00 98 96 80
fee_scale: 00 64
recipient: 05 54 9c 6d f7 b3 76 77 1b 19 ff 3b db 58 d0 4b 49 99 91 66 3c 47 44 4e 42 5f
length of attachment: 00 03
attachment: 31 32 33

```

And then combine togather:

```
02 15 7a 9d 02 ac 57 d4 20 00 00 00 00 3b 9a ca 00 00 00 00 00 00 98 96 80 00 64 05 54 9c 6d f7 b3 76 77 1b 19 ff 3b db 58 d0 4b 49 99 91 66 3c 47 44 4e 42 5f 00 03 31 32 33
```

Finally, we used ed25519 of [curve25519 library](https://github.com/tgalal/python-axolotl-curve25519) to signature. 

```
(For reference only. The signature will be different if generate again)
72 74 61 73 6d 50 31 4c 63 48 79 5a 63 71 35 36 67 52 34 78 57 45 35 78 68 54 78 59 35 33 6f 6f 6f 4d 32 53 61 36 63 75 42 52 61 78 72 71 33 39 63 54 56 6b 39 4d 67 4d 76 6e 38 61 5a 45 6d 4e 78 6b 56 39 55 39 63 62 41 6a 43 50 4d 68 48 46 6f 51 33 57 69 66 57
```

And then encode with base58 and put in `signature` field. 

```
{
  "amount": 1000000000,
  "fee": 10000000,
  "feeScale": 100,
  "timestamp": 1547722056762119200,
  "recipient": "AU6GsBinGPqW8zUuvmjgwpBNLfyyTU3p83Q",
  "senderPublicKey": "B2Khd89jtnpuzGdnyGRcnKycZMBCo6PsotFcWWi1wMDV",
  "attachment": "HXRC",
  "signature": "rtasmP1LcHyZcq56gR4xWE5xhTxY53oooM2Sa6cuBRaxrq39cTVk9MgMvn8aZEmNxkV9U9cbAjCPMhHFoQ3WifW"
}
```

Pass this JSON to full node. And full node broadcasts to network with API `/vsys/broadcast/payment`.


## FAQ
* [[Exchange Integration FAQ]]

## Tools for keys and wallet generation

If using BIP44, please set coin_type=360 ("44'/360'/[account]'/0/[address index]")
* [Wallet Generator](https://github.com/virtualeconomy/v-wallet-generator) (Scala version, similar to BIP39)
* [V HD Key](https://github.com/virtualeconomy/v-hdkey-java) (Java version, similar to BIP32)
* [V HD Key](https://github.com/virtualeconomy/v-hdkey-go) (Go version, similar to BIP32)
* [pyvsystems](https://github.com/virtualeconomy/pyvsystems) SDK (Python version)