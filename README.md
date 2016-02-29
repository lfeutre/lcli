# lcli

[![][lcli-logo]][lcli-logo-large]

[lcli-logo]: resources/images/elkly-250x.png
[lcli-logo-large]: resources/images/elkly-1200x.png

*An LFE Command Line Options & Tool Builder*


#### Table of Contents

* [Introduction](#introduction-)
* [Documentation](#documentation-)
* [Example Usage](#example-usage-)
* [License](#license-)


**NOTICE**: This is a work in progress


## Introduction [&#x219F;](#table-of-contents)

lcli (pronounced "Elkly") is a semi-opinionated library for creating command line tools and supporting options (flags) and arguments (non-flag parameters) parsing. Regular options parsing is done using the Erlang [getopt](https://github.com/oubiwann/getopt-erl) community library. lcli manages the handling of commands and nested subcommands.

lcli is very opinionated around context: sometimes the complete context for a command is needed, even down at the lowest (most-nested) level. As such, this should always be made available to the programmer/script writer. (This is a lesson-learned from some of the Common Lisp command line pasring libraries which don't provide the entire context at any given time -- something that can lead to much awkward code.)

lcli is *not* opinionated about *how* option and argument values are to be handled -- possibly not even with a function itself. As such, the specs do not contain or require one to add functions for handling particular options. It is intended that these decisions are managed by whatever ``main`` function (or set of dispatched functions) is (are) in charge of the script itself. This provides the programmer/scripter with maximum flexibility and minimum fuss.


## Documentation [&#x219F;](#table-of-contents)

Project documentation is here:

* [Docs](http://oubiwann.github.com/lcli/current)

Example scripts (some not using lcli, for demonstration purposes) are available here:

* [Examples](examples)

If you've created a simple script that shows off some nice functionality of "Elkly", you are encouraged to submit a PR for inclusion here!


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
      (lfe_io:format "~s~s~n"
                     `(,(proplists:get_value 'greeting opts)
                       ,(proplists:get_value 'greetee opts))))
    (result
      (error result))))

(main)
```

There is a simiar script in the ``examples`` directory (``simple-parse.lfe``), but with better error checking and a ``--help`` option -- be sure to take a look at that one.

You can then do the following with this script, as long as ``lfe`` is in your ``$PATH``:

```cl
$ ./examples/simple-parse.lfe
Hello, World!

$ ./examples/simple-parse.lfe -g "Awwww, "
Awwww, World!

$ ./examples/simple-parse.lfe -e "Mr. Bill!"
Hello, Mr. Bill!

$ ./examples/simple-parse.lfe -g "Awwww, " -e "Nuts!"
Awwww, Nuts!

$ ./examples/simple-parse.lfe --greeting "On, no! " -e "Nuts!"
On, no! Nuts!

$ ./examples/simple-parse.lfe --greeting "On, no! " --greetee "Mr. Bill!"
On, no! Mr. Bill!
```


### Subcommands

More sophisticated usage can create subcommands, etc., for creating command line tools with potentially vast arrays of functionality:

```cl

```


## License [&#x219F;](#table-of-contents)

```
Copyright © 2016 Duncan McGreggor

Distributed under the Apache License, Version 2.0.
```
