#!/usr/bin/env lfe

(defun command ()
  `#m(name "git clone"
      options (#m(long "help" help "Display help text")
               #m(long "local" short #\l
                  help "When the repository to clone from is on a local machine")
               #m(long "verbose" short #\v
                  help "Run verbosely.")
               #m(long "quiet" short #\q
                  help "Operate quietly.")
               #m(long "origin" short #\o type string
                  help "Instead of using the remote name")
               #m(long "branch" short #\b type string
                  help "Instead of pointing the newly created HEAD"))
      args (#m(name "repository" required true
               help "The (possibly remote) repository to clone from.")
            #m(name "directory"
               help "The name of a new directory to clone into."))
      help #m(title "Clone a repository into a new directory"
              description ,(++ "Clones a repository into a newly created directory, "
                    "creates remote-tracking branches for each branch in the "
                    "cloned repository (visible using git branch --remotes), "
                    "and creates and checks out an initial branch that is "
                    "forked from the cloned repository's currently active"
                    "branch."))))

(defun main ()
  (lcli-cmds:usage (command)))

(main)
