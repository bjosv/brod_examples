# brod_examples
Repo with a collection of different usage scenarios of the Erlang Kafka client 'brod'

## Run test examples

```
make test-env
make test
```

## List topics manually

```
docker exec kafka-1 bash -c "/opt/kafka/bin/kafka-topics.sh --zookeeper localhost --list"
```

## Describe a topic manually

```
docker exec kafka-1 bash -c "/opt/kafka/bin/kafka-topics.sh --zookeeper localhost --describe --topic test-topic"
```
