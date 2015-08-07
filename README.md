`hpp` is a Haskell pre-processor that is also a C90-compatible
pre-processor (with the addition of a `-c-compat` flag). It is
packaged as both a library and an executable.

To use as a Haskell preprocessor for resolving `#ifdef` conditionals
and simple macro expansion while still allowing multi-line string
literals, an invocation might look like,

```
hpp -DDEBUG Foo.hs
```

To use as a C preprocessor, an invocation might look like,

```
hpp -DDEBUG -c-compat foo.c
```
