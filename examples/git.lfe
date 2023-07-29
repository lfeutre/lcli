#!/usr/bin/env lfe
;;;; This is one of the more complicated examples, showcasing an app with its
;;;; commands, subcommands, option composition, etc. If this is more than what
;;;; you need, consider looking at one of the other examples.

(include-lib "lcli/include/records.lfe")

(lcli:start)

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
      name "clone"
      title "Clone a repository into a new directory"
      desc ,(++ "Clones a repository into a newly created directory, "
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
      name "init"
      title "Create an empty Git repository or reinitialize an existing one"
      desc ,(++ "This command creates an empty Git repository - "
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
      title "the stupid content tracker"
      desc ,(++ "Git is a fast, scalable, distributed revision control "
                "system with an unusually rich command set that provides "
                "both high-level operations and full access to internals.")
      options (,(help)
               ,(verbose)
               #m(type option long "version"
                  help "show the version")
               #m(type option long "namespace"
                  help "set the namespace"))
      args (#m(type arg name "command" required? true
               help "the command to execute (see under 'COMMANDS' below)")
            #m(type arg name "args"
               help "additional arguments required or accepted by the command (see 'git <command> --help')"))
      commands (,(git-init)
                ,(git-clone)
                #m(type command
                   name "add"
                   title "Add file contents to the index")
                #m(type command
                   name "mv"
                   title "Move or rename a file, a directory, or a symlink")
                #m(type command
                   name "branch"
                   title "List, create, or delete branches")
                #m(type command
                   name "commit"
                   title "Record changes to the repository"))))

;;; Entrypoint

(defun main ()
  (let ((opts (app)))
    (case (lcli:parse opts)
      ((match-parsed options `#m(help true))
       (lcli:usage opts)
       (halt 0))
      ((match-parsed app a commands c options os args as error '())
       (lfe_io:format "App: ~p~nCommands: ~p~nOptions: ~p~nArgs: ~p~n"
                      (list a c os as))
       (halt 0))
      ((match-parsed error err)
       (io:format "~n~s~n~n" (list err))))))

(main)

