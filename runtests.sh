#!/bin/bash
./rusttojs test/arguments.rs|diff - test/arguments.js
./rusttojs test/main.rs|diff - test/main.js
./rusttojs test/print-number.rs|diff - test/print-number.js
