all: build push

# Registry where to push the images
REGISTRY = liris-vm-27:5050

build:
	docker build -t zookeeper .
	docker tag -f zookeeper $(REGISTRY)/zookeeper

push: build
	docker push $(REGISTRY)/zookeeper

clean:
	docker rmi $(REGISTRY)/zookeeper || :