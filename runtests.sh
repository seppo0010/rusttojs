#!/bin/bash
./rusttojs test/arguments.rs|diff - test/arguments.js
./rusttojs test/main.rs|diff - test/main.js
