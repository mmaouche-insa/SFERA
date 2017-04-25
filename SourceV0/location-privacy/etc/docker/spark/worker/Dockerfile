FROM spark-base:latest

ADD start.sh /
ADD log4j.properties $SPARK_HOME/conf/log4j.properties
EXPOSE 8081

VOLUME /data

ENTRYPOINT ["/start.sh"]