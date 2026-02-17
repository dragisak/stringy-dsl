![stringy-dsl](https://github.com/dragisak/stringy-dsl/actions/workflows/ci.yaml/badge.svg)

# Parsers

# Compile and test
```
sbtn test
```

# CLI demo

Evaluate one program (single or multi-line) with optional `key=value` bindings:

```
sbt "run \"3 + 4 + a.b\" a.b=10"
sbt "run \"if ( is_enabled == true ) { 1 } else { 0 }\" is_enabled=true"
sbt "run \"organization.v1 + suffix\" organization.v1=hello suffix=world"
sbt "run $'var i = 0\ni++'"
```

Show help:

```
sbt "run --help"
```

# Code style

## Check
```
sbtn scalafmtCheckAll
```

## Reformat
```
sbtn scalafmtAll
```
