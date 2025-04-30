#! /bin/bash
set -e
docker build -t bollama_static .
docker run --rm -v "$(pwd):/output" bollama_static cp /bollama/bollama /output/bollama_static
