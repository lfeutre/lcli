(defmodule lcli-cmds-tests
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
    #(commands #\c "commands" string "Comma-separated commands.")))

(defun specs-4 ()
  `(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")
    #(commands (#(db #(opts (#(help #\h "help" undefined "Display help for 'db' command.")))))
                #(app #(opts (#(help #\h "help" undefined "Display help for 'app' command.")))))))

(deftest has-commands?
  (is-equal 'false (lcli-cmds:has-commands? (specs-1)))
  (is-equal 'false (lcli-cmds:has-commands? (specs-2)))
  (is-equal 'false (lcli-cmds:has-commands? (specs-3)))
  (is-equal 'true (lcli-cmds:has-commands? (specs-4))))

(deftest commands?
  (is-equal '(false) (lists:map #'lcli-cmds:commands?/1 (specs-1)))
  (is-equal '(false false) (lists:map #'lcli-cmds:commands?/1 (specs-2)))
  (is-equal '(false false false) (lists:map #'lcli-cmds:commands?/1 (specs-3)))
  (is-equal '(false false true) (lists:map #'lcli-cmds:commands?/1 (specs-4))))

(deftest not-commands?
  (is-equal (specs-1) (lists:filtermap #'lcli-cmds:not-commands?/1 (specs-1)))
  (is-equal (specs-2) (lists:filtermap #'lcli-cmds:not-commands?/1 (specs-2)))
  (is-equal (specs-3) (lists:filtermap #'lcli-cmds:not-commands?/1 (specs-3)))
  (is-equal (specs-2) (lists:filtermap #'lcli-cmds:not-commands?/1 (specs-4))))
