# zettel

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
[![GitHub CI](https://github.com/bolt12/zettel/workflows/CI/badge.svg)](https://github.com/bolt12/zettel/actions)
[![Build status](https://img.shields.io/travis/bolt12/zettel.svg?logo=travis)](https://travis-ci.org/bolt12/zettel)
[![Hackage](https://img.shields.io/hackage/v/zettel.svg?logo=haskell)](https://hackage.haskell.org/package/zettel)
[![Stackage Lts](http://stackage.org/package/zettel/badge/lts)](http://stackage.org/lts/package/zettel)
[![Stackage Nightly](http://stackage.org/package/zettel/badge/nightly)](http://stackage.org/nightly/package/zettel)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

`zettel` is a [Zettelkasten](https://writingcooperative.com/zettelkasten-how-one-german-scholar-was-so-freakishly-productive-997e4e0ca125) file processor.

## Requirements

### Dependencies

- GHC
- Cabal / Stack
- Neo4J version 3.X (Version 4 is not supported)

### Config file

`zettel` will attempt to read from `HOME/.config/zettel/zettel-conf` a file which has the
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
Zettelkasten processor

Usage: zettel (new | list | find | delete | edit)

Available options:
-h,--help                Show this help text

Available commands:
new
list
find
delete
edit
```

The `new` command will open your default editor so you can write your zettelkasten. On
save the file will be parsed and insert it on the DB.

The `list` command requires a `--size=` flag and lists all your zettels.

The `find` command requires a `--tags` flag and finds all zettels that have at least one
of the specified tags in common. __NOTE:__ to search for multiple tags do `zettel find
--tags tag1 --tags tag2`, for example.

The `delete` command requires a `--did` flag and deletes the zettel with the specified id.

The `edit` command requires a `--eid` flag and opens the zettel file of the specified id.
__NOTE:__ If any zettel has a connection to the zettel to be edited than it is not
possible to edit that zettel (Editing a zettel will not change its timestamp).

Your Zettels will be stored in `HOME/.config/zettel/`.

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
