all: spark zeppelin
push: push-spark
.PHONY: push push-spark spark

# To bump the Spark version, bump the version in base/Dockerfile, bump
# the version in zeppelin/Dockerfile, bump this tag and reset to
# v1. You should also double check the native Hadoop libs at that
# point (we grab the 2.6.1 libs, which are appropriate for
# 1.5.1-with-2.6).
TAG = 1.6.0_v1

# Registry where to push the images
REGISTRY = liris-vm-27:5050

spark: spark-master spark-worker spark-driver

spark-base:
	docker build -t spark-base base
	docker tag -f spark-base $(REGISTRY)/spark-base
	docker tag -f spark-base $(REGISTRY)/spark-base:$(TAG)

spark-master: spark-base
	docker build -t spark-master master
	docker tag -f spark-master $(REGISTRY)/spark-master
	docker tag -f spark-master $(REGISTRY)/spark-master:$(TAG)

spark-worker: spark-base
	docker build -t spark-worker worker
	docker tag -f spark-worker $(REGISTRY)/spark-worker
	docker tag -f spark-worker $(REGISTRY)/spark-worker:$(TAG)

push-spark: spark
	docker push $(REGISTRY)/spark-base
	docker push $(REGISTRY)/spark-base:$(TAG)
	docker push $(REGISTRY)/spark-worker
	docker push $(REGISTRY)/spark-worker:$(TAG)
	docker push $(REGISTRY)/spark-master
	docker push $(REGISTRY)/spark-master:$(TAG)

clean:
	docker rmi $(REGISTRY)/spark-worker:$(TAG) || :
	docker rmi $(REGISTRY)/spark-worker || :
	docker rmi $(REGISTRY)/spark-master:$(TAG)  || :
	docker rmi $(REGISTRY)/spark-master || :
	docker rmi $(REGISTRY)/spark-base:$(TAG) || :
	docker rmi $(REGISTRY)/spark-base || :