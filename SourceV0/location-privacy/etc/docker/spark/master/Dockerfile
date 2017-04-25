FROM spark-base:latest

ADD start.sh /
ADD log4j.properties $SPARK_HOME/conf/log4j.properties
EXPOSE 30100 8080

ENTRYPOINT ["/start.sh"]