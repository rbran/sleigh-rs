## Sleigh-rs

This project is a Ghidra Sleigh parser.

The project is unfinished, and is not ready for use.

For an example on how this lib can be used, checkout
[sleigh2rust](https://github.com/rbran/sleigh2rust) and [sleigh3rust](https://github.com/rbran/sleigh3rust).

## Ghidra Sleigh

The Sleigh language used by Ghidra, describes CPU instructions sets,
designed to facilitate the reverse-engineering and and emulate cpu architectures.

## TODO

* Implement token parsing, instead of parsing the syntax directly from string.
* Preprocessor should output tokens instead of strings.
* Implement the ordering or table constructors.
* Verification of the Assembly pattern matching blocks.
* Unit tests.
* Discover: how to bit-constrain the table pattern for ordering in a expansive
block that have ellipsis on the left and right.

