#!/usr/bin/env sh
##
## This script runs a Cardano Node on the specified network (preprod, preview, or mainnet).
## Usage: ./node-runner.sh {preprod|preview|mainnet}
## Set the network based on the first argument
##
## We assume that the cardano-node binary is available in the PATH
##
case "$1" in
    "preprod")
        echo "Starting cardano-node on preprod network ..."
        NETWORK='preprod'
        ;;
    "preview")
        echo "Starting cardano-node on preview network ..."
        NETWORK='preview'
        ;;
    "mainnet")
        echo "Starting cardano-node on mainnet network ..."
        NETWORK='mainnet'
        ;;
    *)
        echo "Usage: $0 {preprod|preview|mainnet}"
        exit 1
        ;;
esac

DIR=$(pwd)
NODE_CONFIG_DIR=$DIR/share
VAR=$DIR/var/$NETWORK/$(date +"%Y-%m-%d")
PORT=6000

#
##################################################
#  uncomment the following lines to back up existing database folder
#  This is useful if you want to start a fresh node but keep a backup of the existing database
##################################################
#
# if [ -d $VAR ]; then
#     echo "backup of $VAR"
#     mv $VAR $VAR-$(date +"%s")-backup
# fi

mkdir -p "$VAR"/db

# Set a variable to indicate the local IP address of the computer where Cardano Node runs
# 0.0.0.0 listens on all local IP addresses for the computer
HOSTADDR=0.0.0.0
# Set a variable to indicate the file path to your topology file
TOPOLOGY=$NODE_CONFIG_DIR/$NETWORK/topology.json
# Set a variable to indicate the folder where Cardano Node stores blockchain data
DB_PATH=$VAR/db
# Set a variable to indicate the path to the Cardano Node socket for Inter-process communication (IPC)
SOCKET_PATH=$VAR/socket
# Set a variable to indicate the file path to your main Cardano Node configuration file
CONFIG=$NODE_CONFIG_DIR/$NETWORK/config.json
#
# Run Cardano Node using the options that you set using variables
#
cardano-node run \
    --topology ${TOPOLOGY} \
    --database-path ${DB_PATH} \
    --socket-path ${SOCKET_PATH} \
    --host-addr ${HOSTADDR} \
    --port ${PORT} \
    --config ${CONFIG}
