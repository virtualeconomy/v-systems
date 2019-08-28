[![Codacy Badge](https://api.codacy.com/project/badge/Grade/22c2e0ff582f4903b1e2da0fdc0b3fd8)](https://app.codacy.com/app/zl730/v-systems?utm_source=github.com&utm_medium=referral&utm_content=virtualeconomy/v-systems&utm_campaign=Badge_Grade_Settings)
# V Systems  [![Build Status](https://travis-ci.org/virtualeconomy/v-systems.svg?branch=master)](https://travis-ci.org/virtualeconomy/v-systems) [![Code Coverage](https://codecov.io/gh/virtualeconomy/v-systems/branch/master/graph/badge.svg?sanitize=true)](https://codecov.io/gh/virtualeconomy/v-systems/branch/master/graph/badge.svg?sanitize=true)

The code in master branch is under development. The latest release for each network can be found in the [Releases section](https://github.com/virtualeconomy/vsys/releases). You can switch to the corresponding tag and build the application.

# Installation

[How to configure V SYSTEMS Testnet node](https://github.com/virtualeconomy/v-systems/wiki/How-to-Install-V-Systems-Testnet-Node)

[How to configure V SYSTEMS Mainnet node](https://github.com/virtualeconomy/v-systems/wiki/How-to-install-V-Systems-mainnet-Node)

## Compiling Packages from source

It is only possible to create deb and fat jar packages.

### Install SBT (Scala Build Tool)

For Ubuntu/Debian:

```
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
sudo apt-get update
sudo apt-get install sbt
```

You can install sbt on Mac OS X using Homebrew.

### Create Package

Clone this repo and execute

```
sbt packageAll
```

.deb and .jar packages will be in /package folder. To build testnet .deb packages use

```
sbt -Dnetwork=testnet packageAll
```

# Running Tests

Execute 

`sbt test`

**Note**

If you prefer to work with _SBT_ in the interactive mode, open it with settings:
```bash
SBT_OPTS="${SBT_OPTS} -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled" sbt
```

to solve the `Metaspace error` problem.
