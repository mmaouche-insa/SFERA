#!/bin/bash

. /start-common.sh

# Run spark-class directly so that when it exits (or crashes), the pod restarts.
spark-class org.apache.spark.deploy.worker.Worker --webui-port 8081 "$@" spark://spark-master:30100