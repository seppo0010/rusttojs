#!/bin/bash
./rusttojs test/arguments.rs|diff - test/arguments.js
./rusttojs test/helloworld.rs|diff - test/helloworld.js
./rusttojs test/let.rs|diff - test/let.js
./rusttojs test/main.rs|diff - test/main.js
./rusttojs test/print-number.rs|diff - test/print-number.js
