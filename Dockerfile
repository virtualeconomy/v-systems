# This Dockerfile builds from source code so that it aids with Coinbase Rosetta compliance:  https://www.rosetta-api.org/docs/node_deployment.html
# The next step in Rosetta compliance is building an API adapter that meets the tests issued by their CLI tool.  After that, it is possible to submit our Dockerfile to Coinbase as requested. 

# Changed from debian 10 to Ubuntu because debian 10 no longer ships openjdk-8-jre-headless due to a security issue.
FROM ubuntu:20.04

# INSTALL DEPENDENCIES
RUN apt update ; \
    apt install -y ca-certificates gnupg ; \
    echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list ; \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 ; \
    apt update ; \ 
    apt upgrade -y ; \
    apt install -y openjdk-8-jre-headless sbt git

# Compile VSYS
RUN git clone https://github.com/virtualeconomy/v-systems ; \
    cd v-systems ; \
    sbt packageAll


# known-good run command: java -Xmx4096m -Dscala.concurrent.context.maxExtraThreads=1024 -jar vsys-all-0.3.1.jar ../vsys-mainnet.conf


# Actually Run Vsys

CMD java -Xmx4096m -Dscala.concurrent.context.maxExtraThreads=1024 -jar v-systems/target/vsys-all-*.jar v-systems/vsys-mainnet.conf
