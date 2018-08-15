#!/bin/bash

DEFAULT_NET_IP=`ifconfig eth0 | awk '/inet addr/ {gsub("addr:", "", $2); print $2}'`
VEE_NET_IP=`ifconfig eth1 | awk '/inet addr/ {gsub("addr:", "", $2); print $2}'`

echo Default: $DEFAULT_NET_IP
echo Vee: $VEE_NET_IP
echo Options: $VEE_OPTS

#java -Dvee.network.declared-address=$VEE_NET_IP:$VEE_PORT $VEE_OPTS -jar /opt/vee/vee.jar /opt/vee/template.conf
java -Dvee.network.declared-address=$DEFAULT_NET_IP:9923 $VEE_OPTS -jar /opt/vee/vee.jar /opt/vee/template.conf
