# lcli

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]

*An LFE Command Line Options & Tool Builder*

[![Project logo][logo]][logo-large]

#### Table of Contents

* [Introduction](#introduction-)
* [Differences from lfescript](#differences-from-lfescript-)
* [Example Usage](#example-usage-)
* [Documentation](#documentation-)
* [License](#license-)


**NOTICE**: This is a work in progress


## Introduction [&#x219F;](#table-of-contents)

lcli (pronounced *"Elk-ly"*) is a semi-opinionated library for creating command line tools and supporting options (flags) and arguments (non-flag parameters) parsing. Regular options parsing is done using the Erlang [getopt](https://github.com/oubiwann/getopt-erl) community library. lcli manages the handling of commands and nested subcommands.

lcli is very opinionated around context: sometimes the complete context for a command is needed, even down at the lowest (most-nested) level. As such, this should always be made available to the programmer/script writer. (This is a lesson-learned from some of the Common Lisp command line pasring libraries which don't provide the entire context at any given time -- something that can lead to much awkward code.)

lcli is *not* opinionated about *how* option and argument values are to be handled -- possibly not even with a function itself. As such, the specs do not contain or require one to add functions for handling particular options. It is intended that these decisions are managed by whatever ``main`` function (or set of dispatched functions) is (are) in charge of the script itself. This provides the programmer/scripter with maximum flexibility and minimum fuss.

## Differences from `lfescript` [&#x219F;](#table-of-contents)

This library does not use `lfescript` as its basis, but rather the `lfe` executable itself. As such, a `main` function is not run automatically. Instead, the final line of the script must call the "main" function (whatever its name).

## Example Usage [&#x219F;](#table-of-contents)

### Wrapping ``getopt``

A simple script below demonstrates lcli's wrappage of the Erlang getopt library:

```cl
#!/usr/bin/env lfe

(defun opt-spec ()
  `(#(greeting #\g "greeting" #(string "Hello, ") "A greeting for someone.")
    #(greetee #\e "greetee" #(string "World!") "Someone or something to greet.")))

(defun main ()
  (case (lcli:parse (opt-spec))
    (`(,_ #(opts ,opts) ,_)
      (io:format "~s~s~n"
                 `(,(lcli:get-opt 'greeting opts)
                   ,(lcli:get-opt 'greetee opts)))
    (result
      (error result))))

(main)
```

There is a simiar script in the ``examples`` directory (``simple-parse2.lfe``), but with better error checking and a ``--help`` option -- be sure to take a look at that one.

You can then do the following with this script, as long as ``lfe`` is in your ``$PATH``:

```cl
$ ./examples/simple-parse1.lfe
Hello, World!

$ ./examples/simple-parse1.lfe -g "Awwww, "
Awwww, World!

$ ./examples/simple-parse1.lfe -e "Mr. Bill!"
Hello, Mr. Bill!

$ ./examples/simple-parse1.lfe -g "Awwww, " -e "Nuts!"
Awwww, Nuts!

$ ./examples/simple-parse1.lfe --greeting "On, no! " -e "Nuts!"
On, no! Nuts!

$ ./examples/simple-parse1.lfe --greeting "On, no! " --greetee "Mr. Bill!"
On, no! Mr. Bill!
```


### Subcommands

More sophisticated usage can create subcommands, etc., for creating command line tools with potentially vast arrays of functionality:

```cl

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
