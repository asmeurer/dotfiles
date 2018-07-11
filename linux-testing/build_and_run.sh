#!/bin/bash
set -e
set -x

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path"

# allow x forwarding access from localhost
xhost + 127.0.0.1

docker build -f Dockerfile . -t asmeurer/linux-testing
docker run -e DISPLAY=docker.for.mac.localhost:0 -it asmeurer/linux-testing
