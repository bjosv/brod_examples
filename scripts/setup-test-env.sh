#!/usr/bin/env bash
set -eu

SCRIPTDIR="$(dirname "$0")"

export KAFKA_VERSION="2.2"
echo "Using Kafka version: $KAFKA_VERSION"

# Teardown old and setup new
docker-compose -f $SCRIPTDIR/docker-compose.yml down || true
docker-compose -f $SCRIPTDIR/docker-compose.yml up -d

# Wait for Kafka to be available
wait_sec=4
while [ "$(docker exec kafka-1 bash -c '/opt/kafka/bin/kafka-topics.sh --zookeeper localhost --list')" != '' ]; do
  if [ $wait_sec -le 0 ]; then
    echo "Timeout waiting for kakfa_1"
    exit 1
  fi
  echo "Waiting for kafka_1..."
  wait_sec=$(( wait_sec - 1 ))
  sleep 1
done
