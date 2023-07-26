# Erlang `getopt` Wrapper and More

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
