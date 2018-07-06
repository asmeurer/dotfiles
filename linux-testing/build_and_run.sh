#!/bin/bash
set -e
set -x

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path"

docker build -f Dockerfile . -t asmeurer/linux-testing
docker run -it asmeurer/linux-testing
