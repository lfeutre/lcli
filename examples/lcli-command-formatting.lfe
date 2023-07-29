#!/usr/bin/env lfe

(defun command ()
  `#m(type command
      name "git clone"
      title "Clone a repository into a new directory"
      desc ,(++ "Clones a repository into a newly created directory, "
                    "creates remote-tracking branches for each branch in the "
                    "cloned repository (visible using git branch --remotes), "
                    "and creates and checks out an initial branch that is "
                    "forked from the cloned repository's currently active"
                    "branch.")
      options (#m(type option long "help" help "Display help text")
               #m(type option long "local" short #\l
                  help "When the repository to clone from is on a local machine")
               #m(type option long "verbose" short #\v
                  help "Run verbosely")
               #m(type option long "quiet" short #\q
                  help "Operate quietly")
               #m(type option long "origin" short #\o val-type string
                  help "Instead of using the remote name")
               #m(type option long "branch" short #\b val-type string
                  help "Instead of pointing the newly created HEAD"))
      args (#m(type arg name "repository" required true
               help "The (possibly remote) repository to clone from")
            #m(type arg name "directory"
               help "The name of a new directory to clone into"))))

(defun main ()
  (lcli:usage (command)))

(main)

(io:format "************************************************~n~n~n")
(io:format " Or, this could also be done using lcli records~n")
(io:format "                 (see below ...)~n~n~n")
(io:format "************************************************~n")

(include-lib "lcli/include/records.lfe")

(defun command ()
  (make-command name "git clone"
                title "Clone a repository into a new directory"
                desc (++
                  "Clones a repository into a newly created directory, "
                  "creates remote-tracking branches for each branch in the "
                  "cloned repository (visible using git branch --remotes), "
                  "and creates and checks out an initial branch that is "
                  "forked from the cloned repository's currently active"
                  "branch.")
                options (list
                  (make-option long "help"
                               help "Display help text")
                  (make-option long "local"
                               short #\l
                               help "When the repository to clone from is on a local machine")
                  (make-option long "verbose"
                               short #\v
                               help "Run verbosely")
                  (make-option long "quiet"
                               short #\q
                               help "Operate quietly")
                  (make-option long "origin"
                               short #\o
                               val-type 'string
                               help "Instead of using the remote name")
                  (make-option long "branch"
                               short #\b
                               val-type 'string
                               help "Instead of pointing the newly created"))
                args (list
                  (make-arg name "repository"
                            required 'true
                            help "The (possibly remote) repository to clone from")
                  (make-arg name "directory"
                            help "The name of a new directory to clone into"))))

(defun main ()
  (lcli:usage (command)))

(main)
