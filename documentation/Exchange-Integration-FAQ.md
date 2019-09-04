# Full Node related question
### 1. My full node program is crashed. How to make the node stable?

**Answer:** Please check the log. If it is caused by out of memory, we suggest you increase your RAM. The standard requirement is 16G. For more hardware requirement, please refer to [this part](https://github.com/virtualeconomy/v-systems/wiki/Instructions-for-Exchanges#hardware).

### 2. Any security issue should exchange pay attention?

**Answer:** The **api-key-hash** in config should set to your own password, not using the default hash. And we suggest the exchange modify firewall rule and **not open 9922 in public** network, only for internal network using (But to make communication among the nodes easy and smooth, please keep port 9921 and 9923 opening in public).

# Swagger & cURL related question
### 1. When I called API, I got "Provided API key is not correct" error. How can I solve it?

**Answer:** Firstly, make sure the full node you connect is owned by you. Then you should have set the api-key-hash in config (reference [here](https://github.com/virtualeconomy/v-systems/wiki/Instructions-for-Exchanges#step-2-configuration)). If configure well, remember your orginal API key. 

For swagger, input your API key in top bar and click "Explore". 

For curl, pass your API key in HTTP header. For instance,

```shell
$ curl -X GET --header 'api_key: <your api key>' 'http://<full node ip>:9922/wallet/seed'
```

### 2. Why I always got "The server was not able to produce a timely response to your request. Please try again in a short while!" error with HTTP repsonse 503?

**Answer:** Currently, we only support JSON for interaction. Please check HTTP header. In HTTP header, the "Accept" should be “application/json”, not "text/plain" or any other value.

### 3. I got "The requested resource could not be found but may be available again in the future" error with HTTP repsonse 404. How can I solve it?

**Answer:** Make sure call method (GET/POST) is correct. The curl default method is GET. Remember to add "-X GET" or "-X POST" behind curl command.

### 4. When I call POST /addresses to create account, where is the wallet data store?

**Answer:** In your configuration file, you should have set the **directory**:

```
# V Systems node settings
vsys {
  # Path Settings
  directory = <block data folder path>
  ......
```
Then, all accounts data will store in ```<block data folder path>/wallet/wallet.dat``` file. It would better to backup this file in scheduled task.

# Pyvsys related question
### 1. When I create API wrapper, how to fill the API key field?

**Answer:** Just left API key field empty. Like this,

```python
import pyvsys as pv
my_wrapper = pv.create_api_wrapper('http://<full node ip>:9922', api_key='')
```
Just for payment, API key is not required. Because pyvsys will broadcast the payment with signature which signed by account private key.

### 2. I always get "Failed to get balance detail". How to solved it?

**Answer:** First of all, test the following code:

```
# For Testnet testing
import pyvsys as pv
chain = pv.testnet_chain()
acc = Account(chain=chain)
print(acc.get_info())
```

```
# For Mainnet testing
import pyvsys as pv
node = pv.create_api_wrapper('https://wallet.vee.tech/api', api_key='')
chain = pv.Chain(chain_name='mainnet', chain_id='M', address_version=5, api_wrapper=node)
acc = Account(chain=chain)
print(acc.get_info())
```

If it works, please check your previous code. Otherwise, it should be network issue. Please check the network.

# Balance & Payment related question
### 1. When I check balance, I get a lot of field like 'regular', 'available', 'effective', 'mintingAverage'. Which field is the actual final balance?

**Answer:** The ```available``` balance is the actual balance which account currently could use.

### 2. What is the minimum transaction fee?

**Answer:** The mininum transaction fee is 10000000(0.1 VSYS). 

### 3. How to fill fee scale field?

**Answer:** In current version, the fee scale is a fixed value. It should be 100.

### 4. How many block should we wait for a transaction to be final?

**Answer:** The waiting blocks should be **M * super_node_num + 1** (M = max accept waiting minutes since transaction sent). The greater confirmations you wait, the less false confirm in chain case will be occurred. Currently, super node num is 15. So we suggest the waiting blocks are more than 15. For example, if you just sent a transaction and you could wait 2 minutes to confirm the transaction in chain, then waiting blocks should be 2*15+1=31.