![stringy-dsl](https://github.com/dragisak/stringy-dsl/actions/workflows/ci.yaml/badge.svg)

# Parsers

# Compile and test

```
sbt test
```

# CLI demo

Evaluate one expression with optional `key=value` bindings:

```
sbt "run \"3 + 4 + a.b\" a.b=10"
sbt "run \"if ( is_enabled == true ) { 1 } else { 0 }\" is_enabled=true"
sbt "run \"organization.v1 + suffix\" organization.v1=hello suffix=world"
```

Show help:

```
sbt "run --help"
```

# Code style

```
sbt scalafmtAll
```
