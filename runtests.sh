#!/bin/bash
./rusttojs test/main.rs|diff - test/main.js
