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

TBD


## Documentation [&#x219F;](#table-of-contents)

Project documentation is here:

* [Docs](http://oubiwann.github.com/lcli/current)


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

Distributed under the Apache License Version 2.0.
```
