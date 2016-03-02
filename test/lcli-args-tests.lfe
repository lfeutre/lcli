(defmodule lcli-args-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-raw-args
  (is-equal '(#(script "") #(args ()))
            (lcli-args:get-raw-args '()))
  (is-equal '(#(script "./myscript") #(args ()))
            (lcli-args:get-raw-args '("./myscript")))
  (is-equal '(#(script "./myscript") #(args ("-h")))
            (lcli-args:get-raw-args '("./myscript" "-h")))
  (is-equal '(#(script "./myscript") #(args ("--help" "--all")))
            (lcli-args:get-raw-args '("./myscript" "--help" "--all"))))

(deftest get-script
  (is-equal ""
            (lcli-args:get-script
              (lcli-args:get-raw-args '())))
  (is-equal "./myscript"
            (lcli-args:get-script
              (lcli-args:get-raw-args '("./myscript"))))
  (is-equal "./myscript"
            (lcli-args:get-script
              (lcli-args:get-raw-args '("./myscript" "-h"))))
  (is-equal "./myscript"
            (lcli-args:get-script
              (lcli-args:get-raw-args '("./myscript" "--help" "--all")))))

(deftest get-args
  (is-equal '()
            (lcli-args:get-args
              (lcli-args:get-raw-args '())))
  (is-equal '()
            (lcli-args:get-args
              (lcli-args:get-raw-args '("./myscript"))))
  (is-equal '("-h")
            (lcli-args:get-args
              (lcli-args:get-raw-args '("./myscript" "-h"))))
  (is-equal '("--help" "--all")
            (lcli-args:get-args
              (lcli-args:get-raw-args '("./myscript" "--help" "--all")))))
