#!/bin/sh

# Stop all running containers.
docker kill $(docker ps -q)

# Destroy all containers, images, networks, and volumes.
docker system prune --all --volumes --force
