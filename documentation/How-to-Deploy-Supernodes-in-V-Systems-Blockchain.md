# Introduction
In V Systems, only supernodes have the right to minting blocks and getting V coin rewards. In this wiki we will give you details on deploying a supernode. Additionally, a few recommendations are provided to increase security of the blockchain and the safety of your coins.

# Preparation

## Submit Form

All the Supernode information is list in https://vsysrate.com . To become a Supernode, you need to submit this [application form](https://goo.gl/forms/CVFsD4RpcaGreZtE2) to **declare** your Supernode name, logo, etc. 

## Supernode Hardware

In current stage, the system requirement is **a minimum 16GB of RAM, direct access to SSD and 10 Gbps network**.

The comparable Amazon Web Services (AWS) EC2 instance is i3 large.

It is strongly recommended each supernode maintains a standby node of similar spec.  

The cost of AWS i3 large is $200 ~ $400 per month for 2 servers (supernode and standby node).

Considering hard disk spend of smart contract and decentralized database in the feature, we estimate the server cost will be increased 10% every 2 years.

## Supernode Software

### Operating System

All Java 1.8 and Python runnable operating system are supported (including Ubuntu, CentOS, MacOS, Windows etc.).

The recommended operating system is **Ubuntu 16.04 LTS** (or above).

In this instruction, we'll take Ubuntu 16.04 for instance.

### Service Installation

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

Install Unzip and Git if your machine does not have this tool.

```shell
$ sudo apt-get install unzip
$ sudo apt-get install git-core
```

# Start V Systems full node

### Step 1: Prepare

There are two ways to prepare to node program file. Choose the solution as you want.

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

* It is better to choose more than 3 peers to fill the **known-peers** field. You could check known-peers from default config file ([Testnet Config](https://github.com/virtualeconomy/v-systems/blob/master/vsys-testnet.conf), [Mainnet Config](https://github.com/virtualeconomy/v-systems/blob/master/vsys-mainnet.conf)). Some known-peers for current reference:

	```
	# For TestNet
	known-peers = ["54.193.47.112:9923","13.57.25.133:9923","18.218.106.1:9923","3.17.78.253:9923","34.222.191.174:9923"]
	# For MainNet (contact us to get more known peers)
	known-peers = ["13.55.174.115:9921","52.30.23.41:9921","13.113.98.91:9921","3.121.94.10:9921"]
	```

* The **blockchain.type** should be filled with TESTNET or MAINNET.

* The **enable** in **miner** is used for control minting. If the node is supernode, please set to "yes". Or if it is standby server, please set to "no".

* To ensure minting security, we suggest filling **reward-address** with your cold wallet address. You can download cold wallet in https://v.systems/wallet.html or generated by [wallet generator](https://github.com/virtualeconomy/v-wallet-generator)

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

# The Conditions of Becoming Supernode

Before introducing the conditions, we need declare some concept of balance first:

* The `regular` balance is the coins you hold.
* `available` balance = regular - leased out
* `effective` balance = regular - leases out + leased in
* The `Minting Average Balance` (MAB) is a new concept balance in SPoS. The MAB is related with `Coin Age`. When some coins are just transferred or leased to a new address, the MAB of these coins will be 0 and start to grow when new blocks are generated. The coin age will be full after 86400 blocks generated. That means, with current 15 supernodes generating block, your MAB will reach the max after about 4 days.

**(Important)The conditions that contend successful are as the following:**

* The ```available``` balance >= 50000 V Coins (transaction fee cost).
* The ```effective``` balance >= 1 million V Coins after contend (Supernode minimum effective balance limit).
* The ```mintingAverage``` balance > target supernode's MAB.

# Raise Stake to Increase MAB

Once the node startup, you can check your node status via API (You can open browser and input url `http://<node ip>:9922` to see all APIs).

### Step 1: Check node wallet address

Use HTTP GET calling /addresses API to get all addresses:

```shell
$ curl -X GET 'http://<node ip>:9922/addresses'
```

Response:

```
[
  "ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV"
]
```

**Ask your supporters to lease to your node wallet address** and then the lease-in coins will increase your MAB, until 4 days that MAB reach the max.

### Step 2: Check balance

To check balance of address, use HTTP GET to call /addresses/balance/details/{address}

```shell
$ curl -X GET 'http://<node ip>:9922/addresses/balance/details/ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV'
```

Response:

```
{
	'address': 'ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV',
	'regular': 109010000000, 		# regular balance
	'available': 108910000000,  		# available balance (regular - leased out)
	'effective': 108910000000,  		# effective balance (regular - leases out + leased in)
	'mintingAverage': 108909964800,  	# MAB value
	'height': 643936
}
```

(100000000 = 1 V Coin)

### Step 3: Check MAB of target supernode

Use HTTP GET calling /consensus/allSlotsInfo API to get all supernode `mintingAverageBalance` (MAB) information:

```shell
$ curl -X GET 'http://<node ip>:9922/consensus/allSlotsInfo'
```

Response:

```
[
  {
    "height": 2304399
  },
  {
    "slotId": 0,
    "address": "ATxpELPa3yhE5h4XELxtPrW9TfXPrmYE7ze",
    "mintingAverageBalance": 289999980000000
  },
  {
    "slotId": 1,
    "address": "ATtRykARbyJS1RwNsA6Rn1Um3S7FuVSovHK",
    "mintingAverageBalance": 203029935461020
  },
  {
    "slotId": 2,
    "address": "ATtchuwHVQmNTsRA8ba19juGK9m1gNsUS1V",
    "mintingAverageBalance": 149999999990400
  },
  {
    "slotId": 3,
    "address": "AU4AoB2WzeXiJvgDhCZmr6B7uDqAzGymG3L",
    "mintingAverageBalance": 495476103824140
  },
  ......
]
```

Select one supernode that MAB is lower than yours. If you contend failed due to insufficient MAB, your contending fee would **NOT** be returned. 


# Start Contend Action

When everything is ready (```available``` balance >= 50000 V Coins, ```effective``` balance >= 1 million V Coins, and ```mintingAverage``` balance > target supernode's MAB), you can start to contend now.

### Use Python SDK (Method 1)

If you are familiar with python, we suggest you use [pyvsystems](https://github.com/virtualeconomy/pyvsystems) to contend. The following is the sample code that contend on testnet:

```python
import pyvsystems as pv
from pyvsystems import Account
custom_wrapper = pv.create_api_wrapper('http://<node ip>:9922', api_key='')
ts_chain = pv.Chain(chain_name='testnet', chain_id='T', address_version=5, api_wrapper=custom_wrapper)
my_node = Account(chain=ts_chain, private_key='<your base58 private key>')
my_node.contend(<target slot id>)
```

### Use Swagger (Method 2)

You could open ```http://<node ip>:9922``` in browser.

Use POST /spos/contend to contend. The post json is as the following:

```
{
  "sender": "<your node address>",
  "slotId": <target slot id>,
  "fee": 5000000000000,
  "feeScale": 100
}
```

Note: If you get `Provided API key is not correct` error, please make sure you have filled correct **api_key** on top right of swagger client, and set the correct **api-key-hash** in your config file.

### Check Contend Result

Once contend successfully and your node keeps running, check the balance of **reward-address** and you will get minting reward every minute.

And you can check slot status via API as well. Use HTTP GET calling /consensus/slotInfo/{slotId}:

```shell
$ curl -X GET 'http://<node ip>:9922/consensus/slotInfo/<slot id>'
```

Response:

```
{
  "slotId": <slot id>,
  "address": "ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV",
  "mintingAverageBalance": 299999980000000,
  "height": 604399
}
```
If the `address` becomes your node address, that means you have contended successfully.

# Check List After Contend

### Security Issue
For security reason, we suggest the supernode modify firewall rule and **not open 9922 in public network**, only for internal network using. If you want to use swagger client, you could startup another full node with 9922 port opened. To make communication among the nodes easy and smooth, please keep 9921 port opening. It can reduce block missing rate. 

### Supernode Maintains

After Contend, you should also startup a standby node, so that when your supernode server is down, you can switch to standby node as soon as possible. To switch the server, you can change the **enable** of **miner** in config file, or change wallet data file (wallet data file is located at `<block & wallet data file path>/wallet/wallet.dat`). Keep standby node sync the blocks. When you want to switch the server, just switch config or wallet data file and restart node. We strongly suggest that the maintainer use software program to monitor the Supernode status. Once Supernode is down, it can automatically switch config or wallet data file and restart node. 

### Pay Interest to Leaser

The supporter who leased to your supernode increased your MAB. That makes your supernode harder to be contended. You can define your own rules to pay interest to your supporter. To check the active lease payment, you can use GET /transactions/activeLeaseList/{address} API (Attention: Do not call this API on your supernode server.Because this API would use a lot of server resource and would make loss rate of block generation increased).


```shell
$ curl -X GET 'http://<node ip>:9922/transactions/activeLeaseList/<your node wallet address>'
```

Response:

```
[
  [
    {
      "type": 3,
      "id": "suVzh32ENbQp8xR93e3wJeDuLXWuZR1z1ABCBgGK6EB",
      "fee": 10000000,
      "timestamp": 1551866686000000000,
      "proofs": [
        {
          "proofType": "Curve25519",
          "publicKey": "6Wf58gqyuQRCpD42shE3QroGgyp4NBf3sKUKrUPkUkjd",
          "signature": "V7J3ofPVyLvNNzLcGN7hgkkZBGj1wFmpVvdN7taQGgviVwiFYWYio23oyQ1JUrMuecymTNqXnJVrFPFBG1PX5HY7"
        }
      ],
      "amount": 630000000000,
      "recipient": "ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV",
      "feeScale": 100,
      "status": "Success",
      "feeCharged": 10000000,
      "height": 601221
    },
    {
      "type": 3,
      "id": "b1dc5rbXjrVJu7xbgvMBrfUDSQbJ2hpUdbKV77ZvweM",
      "fee": 10000000,
      "timestamp": 1551949975000000000,
      "proofs": [
        {
          "proofType": "Curve25519",
          "publicKey": "RJ9jFFrJvqNL9TTQCbaf5M4o9h5AMKhYejriUsH4B37a",
          "signature": "5RorSByhxziJeHW9z29eZqhBpYPj6q2hrctmGwct5xLY6o6n4JNUDhr2TGPF84Sarhfb8o6RhhoN15k2oezV8HW2"
        }
      ],
      "amount": 53895000000000,
      "recipient": "ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV",
      "feeScale": 100,
      "status": "Success",
      "feeCharged": 10000000,
      "height": 601801
    },
    ......
  ]
]
```

Calculate the lease contribute with your own rule and pay interest to your supporter who keeping leasing to your supernode!