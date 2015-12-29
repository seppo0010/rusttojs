# rusttojs

Translate rust code into javascript.

## Goal

Generate readable javascript code respecting the intent of the rust code.

### Non-goal

Full compatibility. For that look at Rust's
[RFC #604](https://github.com/rust-lang/rfcs/issues/604).

## Requirements

Bison, Flex, Rust, C Compiler

### Ubuntu

```
sudo apt-get install bison flex gcc
curl -sf https://raw.githubusercontent.com/brson/multirust/master/blastoff.sh | sh
```
