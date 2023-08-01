(defmodule lcli-parse-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lcli/include/records.lfe")

;;; Testing data

(defun legal-commands ()
  '("init" "clone" "add" "mv" "branch" "commit" "parent"))

(defun legal-subcommands ()
  '("child1" "child2"))

(defun legal-subsubcommands ()
  '("grandchild1"))

(defun commands ()
  (lcli-type:maps->records
   '(#m(type command
        name "add"
        title "Add file contents to the index")
     #m(type command
        name "mv"
        title "Move or rename a file, a directory, or a symlink")
     #m(type command
        name "branch"
        title "List, create, or delete branches"
        options (#m(type option long "list" help "List branches")
                 #m(type option long "delete" val-type string help "Delete branch")))
     #m(type command
        name "commit"
        title "Record changes to the repository"))))

;;; Commands tests

(deftest commands-depth-1
  (let* ((args '("arg1" "arg2" "branch" "--delete" "branch-name"))
         (parsed (lcli-parse:commands (commands) args)))
    (is-equal "branch"
              (mref (lists:nth 1 parsed) 'name))
    (let ((parsed-getopt (mref (lists:nth 1 parsed) 'parsed)))
      (is-equal "branch-name" (mref (parsed-options parsed-getopt) 'delete))
      (is-equal '("arg1" "arg2" "branch") (parsed-args parsed-getopt))
      (is-equal '() (parsed-commands parsed-getopt)))
    (is-equal '("--delete" "branch-name")
              (mref (lists:nth 1 parsed) 'args))))

;; XXX (let ((cmds (make-command name "git" commands (commands))))

;;; Command tests

(deftest find-command-app-no-args
  (let ((args '()))
    (is-equal #(() "" '())
              (lcli-parse:find-command (legal-commands) args))))

(deftest find-command-app-args
  (let ((args '("arg1" "arg2")))
    (is-equal #(("arg1" "arg2") "" '())
              (lcli-parse:find-command (legal-commands) args))))

(deftest find-command-app-command
  (let ((args '("init")))
    (is-equal #(() "init" ())
              (lcli-parse:find-command (legal-commands) args))))

(deftest find-command-app-args-command
  (let ((args '("arg1" "arg2" "init")))
    (is-equal #(("arg1" "arg2") "init" ())
              (lcli-parse:find-command (legal-commands) args))))

(deftest find-command-app-command-args
  (let ((args '("mv" "old-name" "new-name")))
    (is-equal #(() "mv" ("old-name" "new-name"))
              (lcli-parse:find-command (legal-commands) args))))

(deftest find-command-app-args-command-args
  (let ((args '("arg1" "arg2" "mv" "old-name" "new-name")))
    (is-equal #(("arg1" "arg2") "mv" ("old-name" "new-name"))
              (lcli-parse:find-command (legal-commands) args))))

(deftest find-command-app-args-subcommands
  (let* ((args '("arg1" "arg2"
                 "parent" "arg3"
                 "child2" "arg4" "arg5" "arg6"
                 "grandchild1" "arg7"))
         (`#(,pre "parent" ,post) (lcli-parse:find-command (legal-commands) args)))
    (is-equal '("arg1" "arg2") pre)
    (is-equal '("arg3" "child2" "arg4" "arg5" "arg6" "grandchild1" "arg7") post)
    (let ((`#(,pre "child2" ,post) (lcli-parse:find-command (legal-subcommands) post)))
      (is-equal '("arg3") pre)
      (is-equal '("arg4" "arg5" "arg6" "grandchild1" "arg7") post)
      (let ((`#(,pre "grandchild1" ,post) (lcli-parse:find-command (legal-subsubcommands) post)))
        (is-equal '("arg4" "arg5" "arg6") pre)
        (is-equal '("arg7") post)))))

;;; Utility function tests

(deftest get-command-def
  (let ((record (lcli-parse:get-command-def (commands) "branch"))) 
    (is-equal "List, create, or delete branches"
              (command-title record))))

(deftest getopt
  (let* ((record (lcli-parse:get-command-def (commands) "branch"))
         (parsed (lcli-parse:getopt record '("branch-name")))) 
    (is-equal '("branch-name")
              (parsed-args parsed))))

(deftest legal-commands
  (is-equal '("add" "mv" "branch" "commit")
            (lcli-parse:legal-commands (commands))))
