## Sleigh-rs main page

Sleigh is a processor specification language created and used by the [Ghidra project](https://github.com/NationalSecurityAgency/ghidra/blob/master/GhidraDocs/languages/html/sleigh.html).

This project implement an parser/interpreter for the Sleigh language fully written in Rust.

### Learned lessons

Some concepts that we take for granted, are some times not respected by some CPUs in some situations.
This section describe situations and assumptions that are not always true, those kind of situations
where learned while implementing the sleigh-rs parser.

* [Assembly is a LIE](lesson/assembly)
