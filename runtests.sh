#!/bin/bash
for FILE in $(ls test/*.js); do
  ./rusttojs ${FILE: 0: ${#FILE}-3}.rs|diff - ${FILE}
done
