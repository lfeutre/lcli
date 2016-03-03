(defmodule lcli-cmds-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun spec-1 ()
  '(#(help #\h "help" undefined "Display help text.")))

(defun spec-2 ()
  '(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")))

(defun spec-3 ()
  '(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")
    #(commands #\c "commands" string "Comma-separated commands.")))

(defun spec-4 ()
  `(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")
    #(commands (#(db #(opts (#(help #\h "help" undefined "Display help for 'db' command.")))))
                #(app #(opts (#(help #\h "help" undefined "Display help for 'app' command.")))))))

(deftest has-commands?
  (is-equal 'false (lcli-cmds:has-commands? (spec-1)))
  (is-equal 'false (lcli-cmds:has-commands? (spec-2)))
  (is-equal 'false (lcli-cmds:has-commands? (spec-3)))
  (is-equal 'true (lcli-cmds:has-commands? (spec-4))))

(deftest commands?
  (is-equal '(false) (lists:map #'lcli-cmds:commands?/1 (spec-1)))
  (is-equal '(false false) (lists:map #'lcli-cmds:commands?/1 (spec-2)))
  (is-equal '(false false false) (lists:map #'lcli-cmds:commands?/1 (spec-3)))
  (is-equal '(false false true) (lists:map #'lcli-cmds:commands?/1 (spec-4))))

(deftest not-commands?
  (is-equal (spec-1) (lists:filtermap #'lcli-cmds:not-commands?/1 (spec-1)))
  (is-equal (spec-2) (lists:filtermap #'lcli-cmds:not-commands?/1 (spec-2)))
  (is-equal (spec-3) (lists:filtermap #'lcli-cmds:not-commands?/1 (spec-3)))
  (is-equal (spec-2) (lists:filtermap #'lcli-cmds:not-commands?/1 (spec-4))))
