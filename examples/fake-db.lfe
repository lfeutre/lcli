#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Before running, ensure that lfe is in your $PATH!
;;;;
;;;; An example run of the script, exercising flags and args:
;;;;
;;;; $ ./examples/fake-db.lfe -x --port 5099 --dbname webapp -o output.dump arg1 arg2
;;;;

(include-lib "logjam/include/logjam.hrl")
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
       (lcli:usage opts))
      ((match-parsed app-name an app a commands c options os args as error '())
       (lfe_io:format "App Name: ~p~nApp: ~p~nCommands: ~p~nOptions: ~p~nArgs: ~p~n"
                      (list an a c os as)))
      ((match-parsed error err)
       (io:format "~n~s~n~n" (list err))
       (halt 1)))
    (halt 0)))

(main)
