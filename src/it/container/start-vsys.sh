#!/bin/bash

# DEFAULT_NET_IP=`ifconfig eth0 | awk '/inet addr/ {gsub("addr:", "", $2); print $2}'`
# VSYS_NET_IP=`ifconfig eth1 | awk '/inet addr/ {gsub("addr:", "", $2); print $2}'`

# echo Default: $DEFAULT_NET_IP
# echo Vsys: $VSYS_NET_IP
# echo Options: $VSYS_OPTS

#java -Dvsys.network.declared-address=$VSYS_NET_IP:$VSYS_PORT $VSYS_OPTS -jar /opt/vsys/vsys.jar /opt/vsys/template.conf
java $VSYS_OPTS -jar /opt/vsys/vsys.jar /opt/vsys/conf/template.conf
