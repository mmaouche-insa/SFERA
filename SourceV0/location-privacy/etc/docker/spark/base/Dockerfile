FROM java:8

# Set configuration vars
ENV MAVEN_VERSION 3.3.9
ENV MAVEN_HOME /opt/maven
ENV HADOOP_VERSION 2.6.1
ENV HADOOP_HOME /opt/hadoop
ENV SPARK_VERSION 1.6.0
ENV SPARK_HOME /opt/spark

# Install Maven
RUN mkdir /src && \
    cd /src && \
    curl http://archive.apache.org/dist/maven/maven-3/$MAVEN_VERSION/binaries/apache-maven-$MAVEN_VERSION-bin.tar.gz | \
    tar -zx apache-maven-$MAVEN_VERSION && \
    mv /src/apache-maven-$MAVEN_VERSION $MAVEN_HOME && \
    ln -s $MAVEN_HOME/bin/mvn /usr/local/bin/mvn && \
    rm -rf /src

# Get Hadoop from US Apache mirror and extract just the native libs.
# (Until we care about running HDFS with these containers, this is all we need.)
RUN mkdir -p /src && \
    cd /src && \
    curl http://www.us.apache.org/dist/hadoop/common/hadoop-${HADOOP_VERSION}/hadoop-${HADOOP_VERSION}.tar.gz | \
    tar -zx hadoop-${HADOOP_VERSION}/lib/native && \
    mv hadoop-${HADOOP_VERSION} ${HADOOP_HOME} && \
    rm -rf /src && \
    echo Hadoop ${HADOOP_VERSION} native libraries installed in ${HADOOP_HOME}/lib/native

# Download and compile Apache Spark. We need to compile it in order to support Scala 2.11
RUN mkdir /src && \
    cd /src && \
    curl http://archive.apache.org/dist/spark/spark-$SPARK_VERSION/spark-$SPARK_VERSION.tgz | \
    tar -zx spark-$SPARK_VERSION && \
    cd spark-$SPARK_VERSION && \
    ./dev/change-scala-version.sh 2.11 && \
    JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64 ./make-distribution.sh --name spark --skip-java-test -Phadoop-2.6 -Dscala-2.11 && \
    mv dist $SPARK_HOME && \
    rm -rf /src /root/* && \
    echo Spark ${SPARK_VERSION} installed in ${SPARK_HOME}

# Add Spark configs
ADD log4j.properties $SPARK_HOME/conf/log4j.properties
ADD core-site.xml $SPARK_HOME/conf/core-site.xml
ADD spark-defaults.conf $SPARK_HOME/conf/spark-defaults.conf

# Add Spark startup script
ADD start-common.sh /

# Change the path to include spark binaries
ENV PATH $PATH:$SPARK_HOME/bin

# Create the directory where events will be stored
RUN mkdir /tmp/spark-events && chmod 777 /tmp/spark-events

WORKDIR $SPARK_HOME