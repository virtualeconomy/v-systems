# 介绍
在V Systems中，只有超级节点才有铸币权。这篇文章将会介绍部署超级节点的具体步骤。另外，还会介绍一些节点运营和资产保管的安全建议。

# 准备

## 提交表格

所有的超级节点信息可以在 https://vsysrate.com 里查到。想要成为超级节点，您需要提交这个[申请表](https://goo.gl/forms/CVFsD4RpcaGreZtE2) **声明**您的超级节点名称，图标等等 

## 超级节点硬件要求

目前阶段，系统配置是**最小16G内存，直存固态硬盘和10Gbps带宽**。

与之相匹配的是i3 large型号的亚马逊云主机（AWS）。

我们强烈建议每一台超级节点都对应配置一台同样型号的备用服务器。

两台i3 large型号的AWS主机（主服务器和备用服务器）一个月成本大概200到400美元。

考虑到将来智能合约和去中心化数据库带来的硬盘消耗，我们预估每2年服务器成本会有10%的提升。

## 超级节点软件要求

### 操作系统

可以是所有 Java 1.8 和 Python 可以运行的任何操作系统 (包括 Ubuntu, CentOS, MacOS, Windows 等)。

我们推荐的操作系统是 **Ubuntu 16.04 LTS** (或其更高版本)。

这篇文档我们以Ubuntu 16.04为例进行介绍。

### 软件服务安装

首先我们更新软件包管理器

```shell
$ sudo apt-get update
```

在主机上安装Java 1.8

```shell
$ sudo apt-get install openjdk-8-jdk
```

检查Java版本（需要删除低版本的Java）

```shell
$ java -version
openjdk version "1.8.0_181"
OpenJDK Runtime Environment (build 1.8.0_181-8u181-b13-0ubuntu0.16.04.1-b13)
OpenJDK 64-Bit Server VM (build 25.181-b13, mixed mode)
```

如果您选择编译我们的源码搭建全节点，需要安装Scala编译工具(SBT)

```shell
$ echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
$ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
$ sudo apt-get update
$ sudo apt-get install sbt
```

如果没有Unzip和Git，请在主机上安装这些软件

```shell
$ sudo apt-get install unzip
$ sudo apt-get install git-core
```

## 启动V全节点

### 第1步: 准备程序

有两种方法准备全节点程序，请任意选择一个方法。

#### 下载编译源代码（方法1）

从GitHub上下载源代码

```shell
$ git clone https://github.com/virtualeconomy/v-systems.git
$ cd vsys
```

用SBT编译源代码。如果您希望编译测试网（Testnet）的V全节点，请运行

```shell
# Compile TestNet V full node
$ sbt -Dnetwork=testnet packageAll
```

如果您希望编译主网（Mainnet）的V全节点，请运行

```shell
# Compile MainNet V full node
$ sbt packageAll
```

编译好的文件将会放在**target/vsys-all-[version].jar**这个位置。复制到你自己的工作目录，例如：

```shell
$ mkdir ../vsys-node
$ cp target/vsys-all-*.jar ../vsys-node/v-systems.jar
$ cd ../vsys-node
```


#### 下载已经编译好的文件（方法2）

如果您不想编译源代码，您也可以选择在 https://github.com/virtualeconomy/v-systems/releases 下载最新的JAR文件。

将**v-systems-[version].jar**保存到您的工作目录。

### 第2步: 配置

设置您的配置文件

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
    # Wallet seed as BASE58 string
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
#### 几个比较关键的配置

* **directory**和**data-directory**应该设为您自己的工作目录。我们建您挂载一个较大的硬盘，然后**data-directory**目录设置到这个硬盘下。

* **known-peers** 这项最好填3个或以上的已知节点。您可以在默认的配置文件里查询这些已知节点（[测试网配置](https://github.com/virtualeconomy/v-systems/blob/master/vsys-testnet.conf)，[主网配置](https://github.com/virtualeconomy/v-systems/blob/master/vsys-mainnet.conf)）。现在正在运行的一些节点有：

	```
	# 测试网
	known-peers = ["54.193.47.112:9923","13.57.25.133:9923","18.218.106.1:9923","3.17.78.253:9923","34.222.191.174:9923"]
	# 主网 (欲知更多节点请和我们联系)
	known-peers = ["13.55.174.115:9921","52.30.23.41:9921","13.113.98.91:9921","3.121.94.10:9921"]
	```

* **blockchain.type** 应该填 TESTNET 或 MAINNET.

* **miner**中的**enable**是控制节点铸币的，主服务器请设为`yes`，备用服务器请保持`no`

* 为确保铸币安全，我们建议**reward-address**配置填为您的冷钱包地址。您可以在这里下载V冷钱包 https://v.systems/wallet.html 或者通过这个程序生成您的冷钱包地址 [wallet generator](https://github.com/virtualeconomy/v-wallet-generator)

* 为安全起见，**api-key-hash**这项最好设置成您自己的哈希值。您可以通过这个命令算出您的api密钥的哈希值：

	```
	curl -X POST -d '<输入任意字符作为您的api密钥>' 'https://test.v.systems/api/utils/hash/secure'
	```

* 最后，我们命名并保存配置文件，例如命名为"vsys.conf"。

### 第3步: 运行

我们建立一个screen并运行

```shell
$ screen -S vsys-node
$ sudo java -jar v-systems*.jar vsys.conf
```

如果需要退出screen，可以在键盘上按 `Ctrl + A + D`。

如果需要再次进入screen看状态，可以运行

```shell
$ screen -x vsys-node
```

# 超级节点竞选条件

在介绍竞选条件之前，先说明几个余额概念：

* 您钱包当前所持有币的余额，叫做`regular`余额。
* 可用余额（`available`）则为持有余额减去租出金额的余额（可用余额 = 持有余额 - 租出金额）。
* 有效余额（`effective`）则为持有余额减去租出金额加上租入金额（有效余额 = 持有余额 - 租出金额 + 租入金额）。
* `Minting Average Balance` (MAB)值是SPoS特有的一个概念。MAB值的计算和币龄有关，当一些币被交易或者租赁到另一个新地址，这些币的币龄将会从0开始计算，随着新的区块生成而开始增长。当经历完86400个区块后，这些币的币龄到满。也就是说，按照目前15个超级节点出块的情况来计算，钱包地址MAB值大概要经历4天时间才能达到最大。

**（重点）普通节点想竞选超级节点成功需要符合以下条件：**

* 可用余额（`available`）>= 50000个V币 (手续费)
* 竞选时，有效余额（`effective`）>= 1百万个V币 (超级节点有效余额的最低门槛)
* 您的MAB值（`mintingAverage`）> 竞选目标节点的MAB值

# 募集资金增加MAB

当节点程序启动后，您可以通过API了解目前节点的状况（您可以打开浏览器输入地址`http://<全节点ip>:9922`打开Swagger客户端查看所有API）。

### 第1步：查询节点钱包地址

用 HTTP 的 GET 请求 /addresses 这个API可以得到当前节点钱包的所有地址:

```shell
$ curl -X GET 'http://<node ip>:9922/addresses'
```

如果成功将返回类似结果:

```
[
  "ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV"
]
```

**您需要把您的节点钱包地址告知给您的超级节点支持者并让他们把币租赁到节点钱包地址上**，这些租赁的币将会增加节点地址的MAB，大概经历4天时间达到最大。

### 第2步：查询节点地址余额

查询这一个地址的余额可以通过 HTTP 的 GET 请求 /addresses/balance/details/{address} 这个API

```shell
$ curl -X GET 'http://<node ip>:9922/addresses/balance/details/ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV'
```

如果成功将返回类似结果: 

```
{
	'address': 'ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV',
	'regular': 109010000000, 		# 持有余额
	'available': 108910000000,  		# 可用余额 (= 持有余额 - 租出金额)
	'effective': 108910000000,  		# 有效余额 (= 持有余额 - 租出金额 + 租入金额)
	'mintingAverage': 108909964800,  	# MAB值
	'height': 643936
}
```
(100000000 = 1个V币)

### 第3步：查询竞选目标节点的MAB值

用 HTTP 的 GET 请求 /consensus/allSlotsInfo 这个API可以查到所有超级节点的 `mintingAverageBalance` (MAB) 信息:

```shell
$ curl -X GET 'http://<node ip>:9922/consensus/allSlotsInfo'
```

如果成功将返回类似结果:

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

您需要选一个MAB比您低的超级节点来竞争。需要特别提醒是，当因为MAB低于目标节点而导致的竞选失败的情况下，竞选所交的手续费将**不会**退回。

# 开始竞选

当一切准备就绪（可用余额>=50000个V币，有效余额>=1百万个V币，您的MAB值>竞选目标节点的MAB值），您就可以开始竞选超级节点了。

### 使用 Python SDK (方法1)

如果您比较熟悉Python，我们建议您用[pyvsystems](https://github.com/virtualeconomy/pyvsystems)来进行竞选。以下是在测试网上竞选的代码范例：

```python
import pyvsystems as pv
from pyvsystems import Account
custom_wrapper = pv.create_api_wrapper('http://<node ip>:9922', api_key='')
ts_chain = pv.Chain(chain_name='testnet', chain_id='T', address_version=5, api_wrapper=custom_wrapper)
my_node = Account(chain=ts_chain, private_key='<your base58 private key>')
my_node.contend(<target slot id>)
```

### 使用 Swagger (方法2)

您可以在浏览器里打开`http://<node ip>:9922`进入Swagger客户端

通过 POST 请求 /spos/contend 这个API来竞选。发送的JSON内容如下所示：

```
{
  "sender": "<您的节点钱包地址>",
  "slotId": <目标slot id>,
  "fee": 5000000000000,
  "feeScale": 100
}
```

注意：如果您发送请求后得到的是`Provided API key is not correct`错误，请确保您在Swagger客户端右上方的输入框输入了正确的**api_key**。同时您的节点的配置文件里**api-key-hash**项填写了正确的哈希值。

### 检查竞选结果

当您竞选成功后，如果您的超级节点保持运行，您可以查看您填写的**reward-address**的余额，每分钟应该都会收到铸币奖励。

当然您也可以通过API来查询铸币槽（slot）所属节点的情况。通过HTTP 的 GET 请求 /consensus/slotInfo/{slotId} 这个API:


```shell
$ curl -X GET 'http://<node ip>:9922/consensus/slotInfo/<slot id>'
```

如果成功将返回类似结果:

```
{
  "slotId": <slot id>,
  "address": "ATy98tPdobDBKA35n5CJed6u3AmxKLT3TTV",
  "mintingAverageBalance": 299999980000000,
  "height": 604399
}
```

如果`address`变成了您的节点钱包地址，说明您竞选成功。

# 竞选后续事项

### 安全问题

为确保铸币安全，我们建议您的超级节点服务器设置防火墙规则，不要将9922端口开放到公网，仅内网使用。如果您需要在公网使用Swagger客户端查询链上信息，您可以另起一个全节点，然后开放9922端口。建议打开9921端口，可以使得节点之间通讯更为发达和通畅，降低掉块率。

### 超级节点的维护

竞选成为超级节点后，为保证铸币一直运行，您应该建立另一台服务器做为灾备服务器，当主服务器发生故障的时候可以马上切换服务器恢复铸币。备用服务器和主服务器相互切换可以通过修改配置文件中miner的enable项（或者切换钱包文件`<directory>/wallet/wallet.dat`）。备用服务器要保持运行，保持区块同步，需要切换的的时候，切换配置文件或者钱包文件然后重启节点程序即可。强烈建议写一个自动化监控切换程序，一旦发现reward address得不到收益或者超级节点高度不增加的时候，自动切换配置文件或者钱包文件并重启节点程序。

### 定期分息

您的超级节点支持者将币租赁给您将增加您的MAB值，增加了其他竞争者抢占您这个铸币槽（slot）的难度。为了让支持者持续的为您保持MAB，您可以制定自己的派息策略来回馈您的支持者。查看正在租赁的的交易信息可以通过GET /transactions/activeLeaseList/{address} 这个API来查询（注意，这个API比较消耗资源，不要在正在出块的主服务器上调用该API，否则会影响出块）：

```shell
$ curl -X GET 'http://<node ip>:9922/transactions/activeLeaseList/<您的节点钱包地址>'
```

如果成功将返回类似结果:

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

计算这些租赁的贡献，用您的派息策略来回馈您的支持者，让支持者持续的在您节点一直租赁。