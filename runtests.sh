#!/bin/bash
set -e
for FILE in $(ls test/*.js); do
  echo -n "${FILE: 0: ${#FILE}-3}.."
  ./rusttojs ${FILE: 0: ${#FILE}-3}.rs|diff - ${FILE}
  echo "OK"
done
