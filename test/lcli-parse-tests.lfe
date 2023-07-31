(defmodule lcli-parse-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lcli/include/records.lfe")

(defun legal-commands ()
  '("init" "clone" "add" "mv" "branch" "commit" "parent"))

(defun legal-subcommands ()
  '("child1" "child2"))

(defun legal-subsubcommands ()
  '("grandchild1"))

(deftest command-app-args
  (let ((args '("arg1" "arg2")))
    (is-equal #(("arg1" "arg2") "" '())
              (lcli-parse:command (legal-commands) args))))

(deftest command-app-command
  (let ((args '("init")))
    (is-equal #(() "init" ())
              (lcli-parse:command (legal-commands) args))))

(deftest command-app-args-command
  (let ((args '("arg1" "arg2" "init")))
    (is-equal #(("arg1" "arg2") "init" ())
              (lcli-parse:command (legal-commands) args))))

(deftest command-app-command-args
  (let ((args '("mv" "old-name" "new-name")))
    (is-equal #(() "mv" ("old-name" "new-name"))
              (lcli-parse:command (legal-commands) args))))

(deftest command-app-args-command-args
  (let ((args '("arg1" "arg2" "mv" "old-name" "new-name")))
    (is-equal #(("arg1" "arg2") "mv" ("old-name" "new-name"))
              (lcli-parse:command (legal-commands) args))))

(deftest app-args-subcommands
  (let* ((args '("arg1" "arg2" "parent" "child2" "grandchild1" "arg3"))
         (`#(,pre "parent" ,post) (lcli-parse:command (legal-commands) args)))
    (is-equal '("arg1" "arg2") pre)
    (is-equal '("child2" "grandchild1" "arg3") post)
    (let ((`#(,pre "child2" ,post) (lcli-parse:command (legal-subcommands) post)))
      (is-equal '() pre)
      (is-equal '("grandchild1" "arg3") post)
      (let ((`#(,pre "grandchild1" ,post) (lcli-parse:command (legal-subsubcommands) post)))
        (is-equal '() pre)
        (is-equal '("arg3") post)))))
