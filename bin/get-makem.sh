#!/bin/bash

set -e

here=$(dirname ${0})

repo_root_url="https://raw.githubusercontent.com/alphapapa/makem.sh/master"

echo "downloading makem.sh ..."
curl -fsSL ${repo_root_url}/makem.sh -o ${here}/makem.sh
chmod +x ${here}/makem.sh
