#!/usr/bin/env lfe

(defun options ()
  '(#m(long "help" help "Display help text")
    #m(long "host" short #\h type string default "localhost"
       help "Database server host")
    #m(long "port" short #\p type integer
       help "Database server port")
    #m(long "dbname" type string default "users"
       help "Database name")
    #m(name xml short #\x help "Output data in XML")
    #m(long "verbose" short #\v type integer help "Verbosity level")
    #m(long "output" short #\o type string help "Output file")))

(defun main ()
  (lfe_io:format "~p~n" `(,(lcli:parse (options)))))

(main)