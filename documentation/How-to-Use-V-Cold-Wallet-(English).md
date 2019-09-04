# How to Use V-cold-wallet
[[如何使用V冷钱包 (中文)]]
## 1. Introduction
Our wallet is used to store VSYS coin in security. The key feature is generating and storing private key offline. With the help of a hot wallet, you can do anything that a wallet should do in a extremely secure manner. To use a cold wallet, you should prepare a standby phone with Android version no older than 4.1.

## 2. Installation
* Download .apk from 
[Release](https://github.com/virtualeconomy/v-cold-android/releases)，
or compile yourself from virtualeconomy/v-cold-android 
* Install on your android phone （tablets are not optimized)
* Keep your phone from internet via **switch off your wifi, bluetooch and cellular from now on and never switch them on forever**.

## 3. Generate Address
* Press on the "plus" button on the right-bottom of your screen
* Press on "generate seed", 15 words (called seed phrase) will show up. **Please memorize them in order** then confirm, or copy them and paste to your notes, but **keep it in safe** otherwise your wallet is danger
* Verify your seed phrase
* Set your password, which is used to encrypt your local wallet (both software and file stored)
* Set number of accounts you want to generate 

## 4. Monitor Address Balance on Hot Wallet
* Have a hot wallet account (not related to a cold wallet and needs to be generated separately)
* Log in your account on hot wallet 
[https://wallet.v.systems](https://wallet.v.systems) (For Mainnet),
[https://test.v.systems](https://test.v.systems) (For Testnet)
* Click on "monitor cold wallet", your camera would switch on 
* Choose an address that you want to monitor on your cold wallet, press the QR button, a QR code would show up
* Scan the QR code with camera of your hot wallet 

## 5. Sign Transaction Offline
* Click "transfer" or "lease" on your hot wallet
* Choose a cold wallet address that you want to transfer or lease from
* Set receiver address and amount, then confirm your transaction information, a QR code would show up
* Press the scanning button on the right top of your cold wallet, scan the QR code that hot wallet has generated
* Confirm the information and press the confirm button, another QR code would show up
* Click "continue" on your hot wallet
* Use your hot wallet camera to scan the QR code on your cold wallet

## 6. Backup and Restore Wallet
### i Backup wallet
* Turn on "Auto Backup" in settings page to backup automatically, or press "plus" button and choose "Backup Wallet" in wallet page to backup manually
### ii Restore by file
* When you reinstalled cold wallet, press "plus" button, then choose "load backup file". Input your password, and all your wallet address would show up
### iii Restore wallet by seed 
* When you first install cold wallet, press "plus" button, then choose "Import seed". As "method 1", input the seed phrase (15 words) that you have memorized, set new password and number of address that you want to restore
### iv Cold wallet migration between devices
* Press "Export Seed" in setting on your old device, a QR code that contains information of seed would show up. Press "plus" button on your new device, choose choose "Import seed". As "method 2", press scanning button, scan the QR code on your old device. Finally, set new password and number of address that you want to restore