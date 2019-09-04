This page is for mainnet. To install testnet node, please go to [[How to install V Systems testnet Node]] 

# Install JRE 1.8

Ubuntu users can use the following commands to install JRE.

```
sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
sudo apt-get -y install oracle-java8-installer
```
Now you can check your JRE installation. Run start console and execute command 
```
java -version
```

If you see

```
java version "1.8.0_74"
Java(TM) SE Runtime Environment (build 1.8.0_74-b02)
Java HotSpot(TM) 64-Bit Server VM (build 25.74-b02, mixed mode)
```
then it is good, you can move to the next step!

But if you get an error, then check your installation and try to find some better tutorials in google.

Note. It's necessary to install Oracle JRE 8 with 64-bit version.

# Installation from deb package
There are two types of deb packages of vsys nodes: with upstart loader and system loader.

## Installation from an Upstart deb package
You should use the deb with Upstart if you have an operating system **Ubuntu < 15.04**.

Just download or build latest vsys deb and install it with 

```
sudo dpkg -i vsys*.deb
```
Now it's time to check your vsys config! 

Upstart users can start the node with 

```
sudo service vsys start 
```
and enable autoload on start with 

```
sudo update-rc.d vsys defaults
```
You can find vsys app logs in /var/log/upstart/ folder like that 

```
tail -f /var/log/upstart/vsys.log
```

Default vsys directory (for wallet, blockchain and other node files) is `/var/lib/vsys/.vsys/`. If you want to change vsys directory, you should change **directory** in `/etc/vsys/vsys.conf`.

```
sudo vi /etc/vsys/vsys.conf
```
```
vsys {
  directory = <set to your path>
  logging-level = DEBUG
  ...
```

## Installation from Systemd deb package
You should use the deb with Systemd if you have an operating system **Ubuntu >= 15.04** or **latest Debian releases**.

Just download latest vsys deb and install it with 

```
sudo dpkg -i vsys*.deb
```

Now it's time to check your vsys config! It's embedded into the deb package and unpacked to /usr/share/vsys/conf/vsys.conf and symlinked to /etc/vsys/vsys.conf. Please read [[V Systems Mainnet Node Configuration File]] and edit vsys config with caution.

Systemd users can start the node with 

```
sudo systemctl start vsys.service 
```
 
 and enable autoload on start with 
 
```
sudo systemctl enable vsys.service
```

Systemd users can find vsys app logs in journald storage like that 

```
journalctl -u vsys.service -f
```
You can read about journald tips here.

Default vsys directory (for wallet, blockchain and other node files) is /var/lib/vsys/.vsys/. If you want to change vsys directory, you should change **directory** in /etc/vsys/vsys.conf 

```
sudo vi /etc/vsys/vsys.conf
```
```
vsys {
  directory = <set to your path>
  logging-level = DEBUG
  ...
```

# Installation for advanced users
Download or build latest version of `vsys.jar` and required configuration file for mainnet to any folder, for example `/opt/vsys`.

Check out the configuration file, it is very important!!! On this depends the safety of your wallet and money.

Just open it via your favorite text editor, read the documentation of the configuration file.

Then start console, navigate to the folder with the jar file with the command 
```
cd /opt/vsys
```
and start vsys node with command 
```
java -jar vsys.jar vsys-config.conf
```
Now you can write a script to run every node, which you like and use it! I hope it's worth it!