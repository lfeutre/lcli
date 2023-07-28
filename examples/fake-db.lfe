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
  (let ((opts (options)))
    (case (lcli:parse opts)
      ((match-parsed options `#m(help true))
       (lcli:usage opts)
       (halt 0))
      (result
       (lfe_io:format "~p~n" `(,result))
       (halt 0)))))

(main)