# Brod examples
Repo with a collection of different usage scenarios of the Erlang Kafka client `brod`

See `test/` for the examples.

## Requirements

* make
* docker-compose
* erlang (for example using kerl)
* rebar3

## Run test examples

```
# Start a Kafka 2.2 cluster using docker-composer
make test-env

# Select a profile:
# old     - uses brod 3.7.5
# current - uses brod 3.9.5 (or later)
# latest  - uses brod 3.9.5 (or later) and kafka_protocol 2.4.0
export PROFILE=old

# Build and run tests for profile
make
make test
```

## List topics

```
docker exec kafka-1 bash -c "/opt/kafka/bin/kafka-topics.sh --zookeeper localhost --list"
```

## Describe topics

```
# Describe all
docker exec kafka-1 bash -c "/opt/kafka/bin/kafka-topics.sh --zookeeper localhost --describe"

# Describe specific topic
docker exec kafka-1 bash -c "/opt/kafka/bin/kafka-topics.sh --zookeeper localhost --describe --topic test-topic"
```
