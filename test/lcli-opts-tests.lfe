(defmodule lcli-opts-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun specs-1 ()
  '(#(help #\h "help" undefined "Display help text.")))

(defun specs-2 ()
  '(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")))

(defun specs-3 ()
  '(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")
    #(commands (#(db #(opts (#(help #\h "help" undefined "Display help for 'db' command."))))
                #(app #(opts (#(help #\h "help" undefined "Display help for 'app' command."))))))))

(defun specs-4 ()
  '(#(help #\h "help" undefined "Display help text.")
    #(verbose #\v "verbose" undefined "Be verbose?")
    #(debug #\d "debug" undefined "Set debug.")
    #(file #\f "file" string "Use this file.")))

(defun specs-5 ()
  '(#m(long "help" help "Display help text")
    #m(long "host" short #\h type string default "localhost"
       help "Database server host")
    #m(long "port" short #\p type integer
       help "Database server port")
    #m(long "dbname" type string default "users"
       help "Database name")
    #m(name xml short #\x help "Output data in XML")
    #m(long "verbose" short #\v type integer help "Verbosity level")
    #m(name file type string help "Output file")))

(defun specs-6 ()
  '(#m(long "help" short #\h help "Display help text.")
    #m(long "file" short #\f type string help "Use this file.")
    #m(commands
       (#(db #(opts (#m(long "help" short #\h help "Display help for 'db' command."))))
        #(app #(opts (#m(long "help" short #\h help "Display help for 'app' command."))))))))

(defun parsed-1 ()
  (lcli-opts:parse (specs-1) '("-h")))

(defun parsed-2 ()
  (lcli-opts:parse (specs-2) '("-h" "-f" "afile.txt")))

(defun parsed-2.1 ()
  (lcli-opts:parse (specs-2) '("--file" "afile.txt")))

(defun parsed-4 ()
  (lcli-opts:parse (specs-4) '("-v" "-f" "afile.txt")))

(defun parsed-4.1 ()
  (lcli-opts:parse (specs-4) '("-h" "-v" "-f" "afile.txt")))

(deftest parse-empty
  (is-equal #m(opts #m() args ())
            (lcli-opts:parse (specs-1) ""))
  (is-equal #m(opts #m() args ())
            (lcli-opts:parse (specs-2) "")))

(deftest parse-bools
  (is-equal #m(opts #m(help true) args ())
            (lcli-opts:parse (specs-1) "-h"))
  (is-equal #m(opts #m(help true
                       verbose true)
               args ())
            (lcli-opts:parse (specs-4) "-h -v"))
  (is-equal #m(opts #m(help true
                       verbose true
                       debug true)
               args ())
            (lcli-opts:parse (specs-4) "-h -v -d")))

(deftest parse-values
  (is-equal #m(opts #m(file "some.txt") args ())
            (lcli-opts:parse (specs-2) "-f some.txt"))
  (is-equal #m(opts #m(file "some.txt") args ())
            (lcli-opts:parse (specs-2) "--file some.txt"))
  (is-equal #m(opts #m(file "some.txt") args ())
            (lcli-opts:parse (specs-3) "--file=some.txt")))

(deftest parse-mixed
  (is-equal #m(opts #m(file "some.txt" help true) args ())
            (lcli-opts:parse (specs-2) "-h -f some.txt"))
  (is-equal #m(opts #m(file "some.txt" help true) args ("arg1" "arg2"))
            (lcli-opts:parse (specs-2) "-h -f some.txt arg1 arg2"))
  (is-equal #m(opts #m(help true
                       verbose true
                       debug true
                       file "some.txt")
               args ())
            (lcli-opts:parse (specs-4) "-h -v -d --file some.txt")))

(deftest parse-maps
  (is-equal #m(opts #m(dbname "users"
                       help true
                       host "localhost")
               args ())
            (lcli-opts:parse (specs-5) "--help"))
  (is-equal #m(opts #m(dbname "users"
                       file "myfile.txt"
                       host "myhost"
                       port 1000
                       verbose 3
                       xml true)
               args ("dummy1" "dummy2"))
            (lcli-opts:parse (specs-5) "-h myhost --port=1000 -x myfile.txt -vvv dummy1 dummy2")))

(deftest get
  (is-equal "afile.txt" (lcli-opts:get 'file (parsed-2)))
  (is-equal "afile.txt" (lcli-opts:get 'file (parsed-2.1))))

(deftest help?
  (is-equal 'true (lcli-opts:help? (parsed-1)))
  (is-equal 'true (lcli-opts:help? (parsed-2)))
  (is-equal 'false (lcli-opts:help? (parsed-2.1)))
  (is-equal 'false (lcli-opts:help? (parsed-4)))
  (is-equal 'true (lcli-opts:help? (parsed-4.1))))
