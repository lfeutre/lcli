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
    #(commands (#(db #(opts (#(help #\h "help" undefined "Display help for 'db' command."))))
                #(app #(opts (#(help #\h "help" undefined "Display help for 'app' command."))))))))

(defun specs-5 ()
  '(#m(long "help" short #\h help "Display help text.")
    #m(long "file" short #\f type string help "Use this file.")
    #m(commands
       (#(db #(opts (#m(long "help" short #\h help "Display help for 'db' command."))))
        #(app #(opts (#m(long "help" short #\h help "Display help for 'app' command."))))))))

(deftest commands?
  (is-equal 'false (lcli-cmds:commands? (specs-1)))
  (is-equal 'false (lcli-cmds:commands? (specs-2)))
  (is-equal 'false (lcli-cmds:commands? (specs-3)))
  (is-equal 'true (lcli-cmds:commands? (specs-4))))

(deftest commands-specs?
  (is-equal '(false) (lists:map #'lcli-cmds:commands?/1 (specs-1)))
  (is-equal '(false false) (lists:map #'lcli-cmds:commands?/1 (specs-2)))
  (is-equal '(false false false) (lists:map #'lcli-cmds:commands?/1 (specs-3)))
  (is-equal '(false false true) (lists:map #'lcli-cmds:commands?/1 (specs-4))))

(deftest commands-maps?
  (is-equal '(false false true) (lists:map #'lcli-cmds:commands?/1 (specs-5))))

(deftest filter-commands
  (is-equal (lcli-spec:->maps (specs-1)) (lcli-cmds:filter (specs-1)))
  (is-equal (lcli-spec:->maps (specs-2)) (lcli-cmds:filter (specs-2)))
  (is-equal (lcli-spec:->maps (specs-2)) (lcli-cmds:filter (specs-4)))
  (is-equal (lcli-spec:->maps (specs-2)) (lcli-cmds:filter (specs-5))))
