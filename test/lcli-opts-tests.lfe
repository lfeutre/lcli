(defmodule lcli-opts-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun spec-1 ()
  '(#(help #\h "help" undefined "Display help text.")))

(defun spec-2 ()
  '(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")))

(defun spec-3 ()
  `(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")
    #(commands (#(db #(opts (#(help #\h "help" undefined "Display help for 'db' command.")))))
                #(app #(opts (#(help #\h "help" undefined "Display help for 'app' command.")))))))

(defun spec-4 ()
  '(#(help #\h "help" undefined "Display help text.")
    #(verbose #\v "verbose" undefined "Be verbose?")
    #(file #\f "file" string "Use this file.")))

(defun opts-1 ()
  (case (lcli-opts:parse-opts (spec-1) '("-h"))
    (`#(,opts ,_args)
      opts)))

(defun opts-2 ()
  (case (lcli-opts:parse-opts (spec-2) '("-h" "-f" "afile.txt"))
    (`#(,opts ,_args)
      opts)))

(defun opts-3 ()
  (case (lcli-opts:parse-opts (spec-2) '("--file" "afile.txt"))
    (`#(,opts ,_args)
      opts)))

(defun opts-4 ()
  (case (lcli-opts:parse-opts (spec-4) '("-v" "-f" "afile.txt"))
    (`#(,opts ,_args)
      opts)))

(defun opts-5 ()
  (case (lcli-opts:parse-opts (spec-4) '("-h" "-v" "-f" "afile.txt"))
    (`#(,opts ,_args)
      opts)))

(deftest parse-opts
  (is-equal #(() ())
            (lcli-opts:parse-opts (spec-1) '()))
  (is-equal #(() ())
            (lcli-opts:parse-opts (spec-2) '()))
  (is-equal #((help) ())
            (lcli-opts:parse-opts (spec-1) '("-h")))
  (is-equal #((#(file "some.txt")) ())
            (lcli-opts:parse-opts (spec-2) '("-f" "some.txt")))
  (is-equal #((#(file "some.txt")) ())
            (lcli-opts:parse-opts (spec-2) '("--file" "some.txt")))
  (is-equal #((#(file "some.txt")) ())
            (lcli-opts:parse-opts (spec-2) '("--file=some.txt")))
  (is-equal #((help #(file "some.txt")) ())
            (lcli-opts:parse-opts (spec-2) '("-h" "-f" "some.txt"))))

(deftest get-opt
  (is-equal "afile.txt" (lcli-opts:get-opt 'file (opts-2)))
  (is-equal "afile.txt" (lcli-opts:get-opt 'file (opts-3))))

(deftest boolean?
  (is-equal 'true (lcli-opts:boolean? 'help (opts-1)))
  (is-equal 'false (lcli-opts:boolean? 'file (opts-2)))
  (is-equal 'true (lcli-opts:boolean? 'help (opts-2)))
  (is-equal 'false (lcli-opts:boolean? 'verbose (opts-2)))
  (is-equal 'false (lcli-opts:boolean? 'file (opts-3)))
  (is-equal 'false (lcli-opts:boolean? 'help (opts-3)))
  (is-equal 'true (lcli-opts:boolean? 'verbose (opts-4)))
  (is-equal 'true (lcli-opts:boolean? 'help (opts-5)))
  (is-equal 'true (lcli-opts:boolean? 'verbose (opts-5))))

(deftest help?
  (is-equal 'true (lcli-opts:help? (opts-1)))
  (is-equal 'true (lcli-opts:help? (opts-2)))
  (is-equal 'false (lcli-opts:help? (opts-3)))
  (is-equal 'false (lcli-opts:help? (opts-4)))
  (is-equal 'true (lcli-opts:help? (opts-5))))

(deftest filter-specs
  (is-equal (spec-1) (lcli-opts:filter-specs (spec-1)))
  (is-equal (spec-2) (lcli-opts:filter-specs (spec-2)))
  (is-equal (spec-2) (lcli-opts:filter-specs (spec-3))))
