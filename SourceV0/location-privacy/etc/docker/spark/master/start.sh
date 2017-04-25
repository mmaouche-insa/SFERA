#!/bin/sh

. /start-common.sh

echo "$(hostname -i) spark-master" >> /etc/hosts

# Run spark-class directly so that when it exits (or crashes), the pod restarts.
spark-class org.apache.spark.deploy.master.Master --host spark-master --port 30100 --webui-port 8080 "$@"