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

(defun options ()
  '(#m(long "help" help "Display help text")
    #m(long "host" short #\h type string default "localhost" help "Database server host")
    #m(long "port" short #\p type integer help "Database server port")
    #m(long "dbname" type string default "users" help "Database name")
    #m(name xml short #\x help "Output data in XML")
    #m(long "verbose" short #\v type integer help "Verbosity level")
    #m(long "output" short #\o type string help "Output file")))

(defun main ()
  (case (lcli:parse (options))
    (`(,_ #(opts #m(help true)) ,_ ,_)
     (lcli:usage (options) "db.lfe")
     (halt 0))
    (result
     (lfe_io:format "~p~n" `(,result))
     (halt 0)))
  'ok)

(main)
```

Test run:

```shell
$ ./examples/db.lfe -x --port 5099 --dbname webapp -o output.dump arg1 arg2
```

Output:

``` cl
(#(cmd "./examples/db.lfe")
 #(opts
   #M(dbname "webapp" host "localhost" output "output.dump"
      port 5099 xml true))
 #(args ("arg1" "arg2"))
 #(cmds undefined))
```

With these components parsed, a script writter or CLI tool developer has all the information needed to override configuration with options provided by a user.

Help text is generated by the `getopt` library:

```shell
 $ ./examples/db.lfe --help
```

Output:

```text
Usage: db.lfe [--help] [-h [<host>]] [-p <port>] [--dbname [<dbname>]]
              [-x] [-v <verbose>] [-o <output>]

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
