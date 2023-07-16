# lcli

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]

*An LFE Command Line Options & Tool Builder*

[![Project logo][logo]][logo-large]

#### Table of Contents

* [Introduction](#introduction-)
* [Erlang getopt Wrapper and More](#erlang-getopt-wrapper-and-more-)
* [Differences from lfescript](#differences-from-lfescript-)
* [Example Usage](#example-usage-)
* [Documentation](#documentation-)
* [License](#license-)


**NOTICE**: This is a work in progress


## Introduction [&#x219F;](#table-of-contents)

lcli (pronounced *"Elk-ly"*) is a semi-opinionated library for creating command line tools and supporting options (flags) and arguments (non-flag parameters) parsing. Regular options parsing is done using the Erlang [getopt](https://github.com/oubiwann/getopt-erl) community library. lcli manages the handling of commands and nested subcommands.

lcli is very opinionated around context: sometimes the complete context for a command is needed, even down at the lowest (most-nested) level. As such, this should always be made available to the programmer/script writer. (This is a lesson-learned from some of the Common Lisp command line pasring libraries which don't provide the entire context at any given time -- something that can lead to much awkward code.)

lcli is *not* opinionated about *how* option and argument values are to be handled -- possibly not even with a function itself. As such, the specs do not contain or require one to add functions for handling particular options. It is intended that these decisions are managed by whatever `main` function (or set of dispatched functions) is (are) in charge of the script itself. This provides the programmer/scripter with maximum flexibility and minimum fuss.

## Erlang `getopt` Wrapper and More [&#x219F;](#table-of-contents)

lcli provides a very thing wrapper around getopt, basically converting to and from the getopt spec tuple with LFE/Erlang maps.

In addition, lcli provides command and subcommand support where the command spec is a superset of the getopt spec (thus allowing us to use the getopt library transparently).

With `lcli`, you have the option of defining specs as maps or as `getopt` specs; each has its own aestheic tradeoffs. Note that with maps, the name is optional (taken from the long option name if missing).

Example map specs:

``` cl
'(#m(long "host" short #\h type string default "localhost"
     help "Database server host")
  #m(long "port" short #\p type integer
     help "Database server port")
  #m(long "dbname" type string default "users"
     help "Database name")
  #m(name xml short #\x help "Output data in XML")
  #m(long "verbose" short #\v type integer help "Verbosity level")
  #m(name file type string help "Output file"))
```

Example `getopt` specs:

``` cl
'(#(host #\h "host" #(string "localhost") "Database server host")
  #(port #\p "port" integer "Database server port")
  #(dbname undefined "dbname"#(string "users") "Database name")
  #(xml #\x "xml" undefined undefined "Output data in XML")
  #(verbose #\v "verbose" integer "Verbosity level")
  #(file undefined undefined string "Output file"))
```

## Differences from `lfescript` [&#x219F;](#table-of-contents)

This library does not use `lfescript` as its basis, but rather the `lfe` executable itself. As such, a `main` function is not run automatically. Instead, the final line of the script must call the entrypoint function.

## Example Usage [&#x219F;](#table-of-contents)

### Wrapping `getopt`

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

Help:

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

### Subcommands

More sophisticated usage can create subcommands, etc., for creating command line tools with potentially vast arrays of functionality:

```cl
TBD
```

## Documentation [&#x219F;](#table-of-contents)

Project documentation is TBD.

Example scripts (some not using lcli, for demonstration purposes) are available here:

* [Examples](examples)

If you've created a simple script that shows off some nice functionality of "Elkly", you are encouraged to submit a PR for inclusion here!


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
