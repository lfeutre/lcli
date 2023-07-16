(defmodule lcli-args-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-raw
  (is-equal '(#(script "") #(args ()))
            (lcli-args:get-raw '()))
  (is-equal '(#(script "./myscript") #(args ()))
            (lcli-args:get-raw '("./myscript")))
  (is-equal '(#(script "./myscript") #(args ("-h")))
            (lcli-args:get-raw '("./myscript" "-h")))
  (is-equal '(#(script "./myscript") #(args ("--help" "--all")))
            (lcli-args:get-raw '("./myscript" "--help" "--all"))))

(deftest get-script
  (is-equal ""
            (lcli-args:get-script
              (lcli-args:get-raw '())))
  (is-equal "./myscript"
            (lcli-args:get-script
              (lcli-args:get-raw '("./myscript"))))
  (is-equal "./myscript"
            (lcli-args:get-script
              (lcli-args:get-raw '("./myscript" "-h"))))
  (is-equal "./myscript"
            (lcli-args:get-script
              (lcli-args:get-raw '("./myscript" "--help" "--all")))))

(deftest get
  (is-equal '()
            (lcli-args:get
              (lcli-args:get-raw '())))
  (is-equal '()
            (lcli-args:get
              (lcli-args:get-raw '("./myscript"))))
  (is-equal '("-h")
            (lcli-args:get
              (lcli-args:get-raw '("./myscript" "-h"))))
  (is-equal '("--help" "--all")
            (lcli-args:get
              (lcli-args:get-raw '("./myscript" "--help" "--all")))))
