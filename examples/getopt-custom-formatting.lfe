#!/usr/bin/env lfe

(defun options ()
  '(#m(long "help" help "Display help text")
    #m(long "local" short #\l
       help "When the repository to clone from is on a local machine")
    #m(long "verbose" short #\v
       help "Run verbosely.")
    #m(long "quiet" short #\q
       help "Operate quietly.")
    #m(long "origin" short #\o type string
       help "Instead of using the remote name")
    #m(long "branch" short #\b type string
       help "Instead of pointing the newly created HEAD")))

(defun opening ()
  (list "\e[G" ; the contorl characters and the spaces after 'NAME' erase the 'Usage:' from getopt
  "NAME  \n\n  git clone - Clone a repository into a new directory\n\nSYNOPSIS\n\n  git clone"))

(defun post-cli () "<repository> [<directory>]")

(defun desc-text () "Clones a repository into a newly created directory, creates remote-tracking branches for each branch in the cloned repository (visible using git branch --remotes), and creates and checks out an initial branch that is forked from the cloned repository's currently active branch.")

(defun desc ()
  (list "DESCRIPTION\n"
        (lutil-text:wrap (desc-text) 72 2)
        "\n\nOPTIONS"))

(defun post-opts () "")

(defun main ()
  (getopt:usage (lcli-spec:maps-> (options))
                (opening)
                (post-cli)
                (desc)
                (post-opts)
                'standard_error)
  (halt 0)
  'ok)

(main)