#!/usr/bin/env lfe

(defun options ()
  '(#m(type option long "help" help "Display help text")
    #m(type option long "local" short #\l
       help "When the repository to clone from is on a local machine")
    #m(type option long "verbose" short #\v
       help "Run verbosely.")
    #m(type option long "quiet" short #\q
       help "Operate quietly.")
    #m(type option long "origin" short #\o val-type string
       help "Instead of using the remote name")
    #m(type option long "branch" short #\b val-type string
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
  (getopt:usage (lcli-getopt:->speclist (options))
                (opening)
                (post-cli)
                (desc)
                (post-opts)
                'standard_error)
  (halt 0)
  'ok)

(main)