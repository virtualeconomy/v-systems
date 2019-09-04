# V Systems Node Configuration Parameters
## TestNet config in DEB-packages
If you use DEB-packages to install a node, they also contain configuration files which override some parameters specific to the network:

https://github.com/virtualeconomy/vsys/blob/master/vsys-testnet.conf

## Wallet and Data Location
`directory` should set the local path of your machine

## Network settings
The `known-peers` parameter stores the list of bootstrap nodes to which your node will establish outgoing connections while initializing. By default it set to list of Testnet nodes.

Use `peers-broadcast-interval` parameter to set the period of time between broadcasts of known peers list to other nodes.

Parameter `black-list-residence-time` could be used to set the period of time for which information about external peer stays in the blacklist.

Parameter `connection-timeout` could be used to change the network communication timeout.

Parameter `port` could be your own port number, the default port number is `9923`

## Wallet Setting

In `wallet` section you can configure wallet built in V Systems node.

Use file parameter to set the path to the wallet file. By default, the path to the file is calculated relative to the base application directory.

Parameter `password` could be used to set the password string to protect the wallet file.

## Blockchain settings
`type` should equal to `TESTNET` in testnet configuration file

## Minter settings
In section minter it is possible to configure parameters of the new blocks generator.

Use `enable` parameter to enable or disable block generation on the node. By default, it’s enabled, but if you disable it your node won’t try to generate new blocks (won’t mine).

Use `quorum` parameter to set the minimum required number of connected peers to enable and start mining of new blocks. It defaults to 1, so your node will start minting as soon as it connects to the first peer in the P2P network. Setting this parameter to 0 will enable off-line generation.

Using `interval-after-last-block-then-generation-is-allowed` parameter you tune your node’s blocks download and generation behaviour. By default, it set to 120h, which means that your node won’t start block generation until it has the last block in the local blockchain not older than 120h. 

`reward-address` is set the reward address of minter. The default is set to none, which means the reward will send to minter's address automatically. User can set the `reward-address` to a cold wallet address in order to improve the security.

## REST API settings
In section rest-api you can set the node’s REST API parameters.

Use `enable` parameter to activate or deactivate REST API.

Parameter `bind-address` could be used to select network interface on which REST API will accept incoming connections.

Parameter `port` could be your own port number, the default port number is `9922`

Parameter `api-key-hash` is the hash code of your api key