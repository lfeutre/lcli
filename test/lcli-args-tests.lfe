(defmodule lcli-args-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest raw
  (is-equal '(#(script "") #(args ()))
            (lcli-args:raw '()))
  (is-equal '(#(script "./myscript") #(args ()))
            (lcli-args:raw '("./myscript")))
  (is-equal '(#(script "./myscript") #(args ("-h")))
            (lcli-args:raw '("./myscript" "-h")))
  (is-equal '(#(script "./myscript") #(args ("--help" "--all")))
            (lcli-args:raw '("./myscript" "--help" "--all"))))

(deftest script
  (is-equal ""
            (lcli-args:script
              (lcli-args:raw '())))
  (is-equal "./myscript"
            (lcli-args:script
              (lcli-args:raw '("./myscript"))))
  (is-equal "./myscript"
            (lcli-args:script
              (lcli-args:raw '("./myscript" "-h"))))
  (is-equal "./myscript"
            (lcli-args:script
              (lcli-args:raw '("./myscript" "--help" "--all")))))

(deftest get
  (is-equal '()
            (lcli-args:get
              (lcli-args:raw '())))
  (is-equal '()
            (lcli-args:get
              (lcli-args:raw '("./myscript"))))
  (is-equal '("-h")
            (lcli-args:get
              (lcli-args:raw '("./myscript" "-h"))))
  (is-equal '("--help" "--all")
            (lcli-args:get
              (lcli-args:raw '("./myscript" "--help" "--all")))))
