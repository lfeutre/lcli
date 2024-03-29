# lcli

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]

*An LFE Command Line Options & Tool Builder*

[![Project logo][logo]][logo-large]

#### Table of Contents

* [Introduction](#introduction-)
* [Example Usage](#example-usage-)
* [Documentation](#documentation-)
* [License](#license-)

## Introduction [&#x219F;](#table-of-contents)

lcli (pronounced *"Elk-ly"*) is a library for creating command line tools through support of parsing commands, options (flags), and arguments (non-flag parameters). Regular options parsing is done using the Erlang [getopt](https://github.com/oubiwann/getopt-erl) community library. lcli manages the handling of everything else. Two excellet libraries have served as inspiration, from two different programming languages: Rust's [clap](https://docs.rs/clap/latest/clap/) and Go's [kong](https://github.com/alecthomas/kong) (with the former having a bigger influence).

## Example Usage [&#x219F;](#table-of-contents)

A simple script below demonstrates lcli's wrappage of the Erlang getopt library:

```cl
#!/usr/bin/env lfe

(include-lib "lcli/include/records.lfe")

(defun options ()
  '(#m(type option long "help" help "Display help text")
    #m(type option long "host" short #\h val-type string default "localhost"
       help "Database server host")
    #m(type option long "port" short #\p val-type integer
       help "Database server port")
    #m(type option long "dbname" val-type string default "users"
       help "Database name")
    #m(type option name xml short #\x help "Output data in XML")
    #m(type option long "verbose" short #\v val-type integer help "Verbosity level")
    #m(type option long "output" short #\o val-type string help "Output file")))

(defun main ()
  (case (lcli:parse (options))
    ((match-parsed app a commands c options os args as)
       (lfe_io:format "App: ~p~nCommands: ~p~nOptions: ~p~nArgs: ~p~n"
                      (list a c os as)))))
  'ok)

(main)
```

For a more thorough checking of parse results, see `./examples/fake-db.lfe`.

Test run:

```shell
$ ./examples/db.lfe -x --port 5099 --dbname webapp -o output.dump arg1 arg2
```

Output:

``` cl
App: "./examples/fake-db.lfe"
Commands: ()
Options: #M(dbname "webapp" host "localhost" output "output.dump"
            port 5099 xml true)
Args: ("arg1" "arg2")
```

With these components parsed, a script writter or CLI tool developer has all the information needed to override configuration with options provided by a user.

Usage text is generated through a combination of lcli templates and formatting functions as well as utility functions provided by the `getopt` library:

```shell
 $ ./examples/db.lfe --help
```

Output:

```text
NAME
  ./examples/fake-db.lfe

SYNOPSIS
  ./examples/fake-db.lfe [--help] [-h [<host>]] [-p <port>]
                         [--dbname [<dbname>]] [-x] [-v <verbose>]
                         [-o <output>]

OPTIONS
  --help         Display help text
  -h, --host     Database server host [default: localhost]
  -p, --port     Database server port
  --dbname       Database name [default: users]
  -x             Output data in XML
  -v, --verbose  Verbosity level
  -o, --output   Output file
```

## Documentation [&#x219F;](#table-of-contents)

Project documentation is [here](https://lfeutre.github.io/lcli/).

## License [&#x219F;](#table-of-contents)

```
Copyright © 2016-2023 Duncan McGreggor

Distributed under the Apache License, Version 2.0.
```

[//]: ---Named-Links---

[logo]: priv/images/elkly-250x.png
[logo-large]: priv/images/elkly-1200x.png
[gh-actions-badge]: https://github.com/lfeutre/lcli/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfeutre/lcli/actions
[github]: https://github.com/lfeutre/lcli
[gitlab]: https://gitlab.com/lfeutre/lcli
[lfe]: https://github.com/lfe/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-21%20to%2026-blue.svg
[versions]: https://github.com/lfeutre/lcli/blob/master/.github/workflows/cicd.yml
[github tags]: https://github.com/lfeutre/lcli/tags
[github tags badge]: https://img.shields.io/github/tag/lfeutre/lcli.svg
[github downloads]: https://img.shields.io/github/downloads/lfeutre/lcli/total.svg
[hex badge]: https://img.shields.io/hexpm/v/lcli.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/lcli
[hex downloads]: https://img.shields.io/hexpm/dt/lcli.svg
