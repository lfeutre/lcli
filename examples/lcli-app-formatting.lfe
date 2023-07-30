#!/usr/bin/env lfe
;;;; This is one of the more complicated examples, showcasing an app with its
;;;; commands, subcommands, option composition, etc. If this is more than what
;;;; you need, consider looking at one of the other examples.

;;; Common options

(defun help () #m(type option long "help" short #\h help "Display help text"))
(defun quiet () #m(type option long "quiet" short #\q help "Operate quietly"))
(defun verbose () #m(type option long "verbose" short #\v help "Run verbosely"))
(defun branch () #m(type option long "branch" short #\b val-type string help "Use the specified name"))

;;; Common args

(defun directory () #m(type arg name "directory" help "The name of a new directory to use"))

;;; Commands

(defun git-clone ()
  `#m(type command
      name "git clone"
      title "Clone a repository into a new directory"
      description ,(++ "Clones a repository into a newly created directory, "
                    "creates remote-tracking branches for each branch in the "
                    "cloned repository (visible using git branch --remotes), "
                    "and creates and checks out an initial branch that is "
                    "forked from the cloned repository's currently active"
                    "branch.")
      options (,(help)
               ,(verbose)
               ,(quiet)
               ,(branch)
               #m(type option long "local" short #\l
                  help "When the repository to clone from is on a local machine")
               #m(type option long "origin" short #\o val-type string
                  help "Instead of using the remote name"))
      args (#m(type arg name "repository" required true
               help "The (possibly remote) repository to clone from")
            ,(directory))))

(defun git-init ()
  `#m(type command
      name "git init"
      title "Create an empty Git repository or reinitialize an existing one"
      description ,(++ "This command creates an empty Git repository - "
                    "basically a .git directory with subdirectories for "
                    "objects, refs/heads, refs/tags, and template files. An "
                    "initial branch without any commits will be created (see "
                    "the --initial-branch option below for its name).")
      options (,(help)
               ,(verbose)
               ,(quiet)
               ,(branch))
      args (,(directory))))

;;; Application / collection of commands

(defun app ()
  `#m(type app
      name "git"
      title "A command line tool for the version control system"
      desc "Just what it says on the tin."
      options (,(help)
               ,(verbose)
               #m(type option long "version" help "show the version")
               #m(type option long "namespace" help "set the namespace"))
      args (#m(type arg name "command" required? true help "the command to execute")
            #m(type arg name "args" help "additional arguments required or accepted by the command"))
      commands (,(git-init)
                ,(git-clone))))

(defun main ()
  (lcli:usage (app)))

(main)

;; (set r (lcli-type:map->record (app)))
;; (include-lib "lcli/include/records.lfe")
;; (lcli-getopt:->speclist (app-options r))

(io:format "************************************************~n~n~n")
(io:format " Or, this could also be done using lcli records~n")
(io:format "                 (see below ...)~n~n~n")
(io:format "************************************************~n")

(include-lib "lcli/include/records.lfe")

;;; Common options

(defun help () (make-option long "help" help "Display help text"))
(defun quiet () (make-option long "quiet" short #\q help "Operate quietly"))
(defun verbose () (make-option long "verbose" short #\v help "Run verbosely"))
(defun branch () (make-option long "branch" short #\b type 'string help "Use the specified name"))

;;; Common args

(defun directory () (make-arg name "directory" help "The name of a new directory to use"))

;;; Commands

(defun git-clone ()
  (make-command name "git clone"
                title "Clone a repository into a new directory"
                desc (++ "Clones a repository into a newly created "
                  "directory, creates remote-tracking branches for each branch "
                  "in thecloned repository (visible using git branch --remotes), "
                  "and creates and checks out an initial branch that is "
                  "forked from the cloned repository's currently active"
                  "branch.")
                options (list
                  (help)
                  (verbose)
                  (quiet)
                  (branch)
                  (make-option long "local"
                               short #\l
                               help "When the repository to clone from is on a local machine")
                  (make-option long "origin"
                               short #\o
                               type 'string
                               help "Instead of using the remote name"))
                args (list
                  (make-arg name "repository"
                            required 'true
                            help "The (possibly remote) repository to clone from")
                  (directory))))

(defun git-init ()
  (make-command name "git init"
                title "Create an empty Git repository or reinitialize an existing one"
                desc (++ "This command creates an empty Git repository - "
                  "basically a .git directory with subdirectories for "
                  "objects, refs/heads, refs/tags, and template files. An "
                  "initial branch without any commits will be created (see "
                  "the --initial-branch option below for its name).")
                options (list
                  (help)
                  (verbose)
                  (quiet)
                  (branch))
                args (list
                  (directory))))

;;; Application / collection of commands

(defun app ()
  (make-app name "git"
            title "A command line tool for the version control system"
            desc "Just what it says on the tin."
            options (list
              (help)
              (verbose)
              (make-option long "version"
                           help "show the version")
              (make-option long "namespace"
                           help "set the namespace"))
            args (list
              (make-arg name "command"
                        required? 'true
                        help "the command to execute")
              (make-arg name "args"
                        help "additional arguments required or accepted by the command"))
            commands (list
              (git-init)
              (git-clone))))

(defun main ()
  (lcli:usage (app)))

(main)
