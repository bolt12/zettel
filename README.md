# zettel

[![GitHub CI](https://github.com/bolt12/zettel/workflows/CI/badge.svg)](https://github.com/bolt12/zettel/actions)
[![Build status](https://img.shields.io/travis/bolt12/zettel.svg?logo=travis)](https://travis-ci.org/bolt12/zettel)
[![Hackage](https://img.shields.io/hackage/v/zettel.svg?logo=haskell)](https://hackage.haskell.org/package/zettel)
[![Stackage Lts](http://stackage.org/package/zettel/badge/lts)](http://stackage.org/lts/package/zettel)
[![Stackage Nightly](http://stackage.org/package/zettel/badge/nightly)](http://stackage.org/nightly/package/zettel)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

`zettel` is a Zettelkasten file processor.

## Requirements

### Dependencies

- GHC
- Cabal / Stack
- Neo4J version 3.X (Version 4 is not supported)

### Config file

`zettel` will attempt to read from `HOME/.zettel/zettel-conf` a file which has the
credentials for the Neo4J DB, in the following format:

```
<user> <pass>
```

If none the directory nor the file exists, `zettel` will create one with default
credentials `neo4j neo4j`.

### Other

`zettel` requires you to have specified a default text editor. To do so please provide one
by exporting VISUAL or EDITOR environment variables:

> export EDITOR=vim

By default it will use `vi`.

## Quick Start

```shell
> zettel -h
>
Zettelkasten processor

Usage: zettel (new | list)

Available options:
-h,--help                Show this help text

Available commands:
new
list
```

The `new` command will open your default editor so you can write your zettelkasten. On
save the file will be parsed and insert it on the DB.

The `list` command requires a `--size=` flag and lists all your zettels.

Your Zettels will be stored in `~/.zettel/`.

## Meta Data

Zettel Metadata follows the YAML syntax as you can see in the `ExampleZettel.md`.

Supported fields:

- `title`: specifies the zettel title
- `authors`: specifies the zettel authors
- `tags`: specifies the zettel tags
- `connections`: specifies the connections to other zettels in the following format
  example:
  ```YAML
  [
    {
      "id": 2,
      "reason": "text"
    }
  ]
  ```

## Beta

Please note that this is still a beta version. Please report any issue via Github Issue.

PRs are welcome!
