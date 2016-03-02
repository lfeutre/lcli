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
