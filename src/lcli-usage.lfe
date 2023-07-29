(defmodule lcli-usage
  ;; constants
  (export
   (template-dir 0)
   (template-file 0)
   (default-width 0)
   (default-indent 0))
  ;; public funs
  (export
   (commands 1)
   (compile 1) (compile 2)
   (description 1) (description 3)
   (options 1) (options 2) (options 4)
   (synopsis 2) (synopsis 3) (synopsis 5)
   (template-file 1)
   (template 0) (template 1))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "lcli/include/records.lfe")

;;; Constants

(defun template-dir () "templates")
(defun default-template () 'manpage)
(defun default-width () 72)
(defun default-indent () 2)

;;; Public functions

(defun template-file ()
  (template-file (default-template)))

(defun template-file (name)
  (io_lib:format "~s/~s/~s.mustache" (list (code:priv_dir 'lcli)
                                           (template-dir)
                                           (atom_to_list name))))

(defun template ()
  (template (template-file)))

(defun template
  ((template-name) (when (is_atom template-name))
   (template (template-file template-name)))
  ((filename) (when (is_list filename))
   (template (list_to_binary filename)))
  ((filename)
   (bbmustache:parse_file filename)))

(defun compile (help)
  (compile help 'manpage))

(defun compile (help template-name)
  (let ((bbm-map `#m(title-heading ,(help-title-heading help)
                     title ,(help-title help)
                     synopsis-heading ,(help-synopsis-heading help)
                     synopsis ,(help-synopsis help)
                     desc-heading ,(help-desc-heading help)
                     desc ,(help-desc help)
                     options-heading ,(help-options-heading help)
                     options ,(help-options help)
                     commands-heading ,(help-commands-heading help)
                     commands ,(help-commands help)
                     additional ,(help-additional help))))
    (bbmustache:compile (template template-name) bbm-map `(#(key_type atom)
                                                           #(escape_fun ,(lambda (x) x))))))

(defun synopsis (cmd options)
  (synopsis cmd options ""))

(defun synopsis (cmd options args)
  (synopsis cmd options args (default-width) (default-indent)))

(defun synopsis (cmd opts args width indent)
    (++ (lcli-getopt:usage-cmd-line cmd opts indent)
        (args-synopsis args)))

(defun description (text)
  (description text (default-width) (default-indent)))

(defun description (text width indent)
  (string:trim (lutil-text:wrap text width indent)
               'both "\n"))

(defun options (options)
  (options options '()))

(defun options (opts args)
  (options opts args (default-width) (default-indent)))

(defun options (opts args width indent)
  (string:trim (lcli-getopt:usage-options opts (args-options args))
               'both "\n"))

(defun commands (cmds)
  (string:trim (lcli-getopt:usage-options '() (cmds-options cmds))
               'both "\n"))

;;; Private functions

(defun args-synopsis (args)
  (lists:foldl #'arg-synopsis/2 "" args))

(defun arg-synopsis
  (((match-arg name n required? r) acc)
   (arg-synopsis n r acc)))

(defun arg-synopsis
  ((name 'true acc)
   (io_lib:format "~s <~s>" (list acc name)))
  ((name _ acc)
   (io_lib:format "~s [<~s>]" (list acc name))))

;;; Formatting options that get passed to getopt

(defun args-options
  (('())
   '())
  ((args)
   (lists:map #'arg-option/1 args)))

(defun arg-option
  ((option) (when (is_map option))
   (arg-option (lcli-type:map->record option)))
  (((match-arg name name help help)) (when (== help ""))
   (arg-option name 'undefined))
  (((match-arg name name help help))
   (arg-option name help)))

(defun arg-option
  ((name 'undefined)
   `#(,(++ "<" name ">") ""))
  ((name help)
   `#(,(++ "<" name ">") ,help)))

(defun cmds-options
  (('())
   '())
  ((cmds)
   (lists:map #'cmd-option/1 cmds)))

(defun cmd-option
  ((option) (when (is_map option))
   (cmd-option (lcli-type:map->record option)))
  (((match-command name name title title)) (when (== title ""))
   (cmd-option name 'undefined))
  (((match-command name name title title))
   (cmd-option name title)))

(defun cmd-option
  ((name 'undefined)
   `#(,name ""))
  ((name title)
   `#(,name ,title)))
