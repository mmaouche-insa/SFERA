FROM java:openjdk-8-jre

RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
    apt-get install -y zookeeper && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 2181
ENTRYPOINT ["/usr/share/zookeeper/bin/zkServer.sh"]
CMD ["start-foreground"]