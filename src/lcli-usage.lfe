(defmodule lcli-usage
  ;; constants
  (export
   (template-dir 0)
   (template-file 0)
   (default-width 0)
   (default-indent 0))
  ;; public funs
  (export
   (compile 1) (compile 2)
   (description 1) (description 3)
   (options 1) (options 2) (options 4)
   (synopsis 2) (synopsis 3) (synopsis 5)
   (template-file 1)
   (template 0) (template 1))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "lcli/include/records.lfe")

;;; Constants

(defun template-dir () "templates")
(defun template-file () 'manpage)
(defun default-width () 72)
(defun default-indent () 2)

;;; Public functions

(defun template-file (name)
  (io_lib:format "~s/~s/~s.mustache" (list (code:priv_dir 'lcli)
                                           (template-dir)
                                           (atom_to_list name))))

(defun template ()
  (template (template-file)))

(defun template
  ((name) (when (is_atom name))
   (template (template-file name)))
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
                     description-heading ,(help-description-heading help)
                     description ,(help-description help)
                     options-heading ,(help-options-heading help)
                     options ,(help-options help)
                     additional ,(help-additional help))))
    (bbmustache:compile (template template-name) bbm-map `(#(key_type atom)
                                                           #(escape_fun ,(lambda (x) x))))))

(defun synopsis (cmd options)
  (synopsis cmd options ""))

(defun synopsis (cmd options args)
  (synopsis cmd options args (default-width) (default-indent)))

(defun synopsis (cmd options args width indent)
  (let ((specs (lcli-spec:-> options 'option))
        (len (+ (length (getopt-usage-prefix)) 1)))
     (wrap-line
      (++ (string:substr (lists:flatten (getopt:usage_cmd_line cmd specs))
                         len)
          (args-synopsis args))
      width indent)))

(defun description (text)
  (description text (default-width) (default-indent)))

(defun description (text width indent)
  (string:trim (lutil-text:wrap text width indent)
               'both "\n"))

(defun options (options)
  (options options '()))

(defun options (options args)
  (options options args (default-width) (default-indent)))

(defun options (options args width indent)
  (let ((specs (lcli-spec:-> options 'option)))
    (string:trim (getopt:usage_options specs (args-options args))
                 'both "\n")))

;;; Private constants

(defun getopt-usage-prefix () "Usage: ")

;;; Private functions

(defun args-synopsis (args)
  (lists:foldl #'arg-synopsis/2 "" args))

(defun arg-synopsis
  (((match-arg name name required required) acc)
   (arg-synopsis name required acc))
  ((`#m(name ,name required true) acc)
   (arg-synopsis name 'true acc))
  ((`#m(name ,name) acc)
   (arg-synopsis name 'false acc)))

(defun arg-synopsis
  ((name 'true acc)
   (io_lib:format "~s <~s>" (list acc name)))
  ((name _ acc)
   (io_lib:format "~s [<~s>]" (list acc name))))

(defun args-options
  (('())
   '())
  ((args)
   (lists:map #'arg-option/1 args)))

(defun arg-option
  (((match-arg name name help help)) (when (== help ""))
   (arg-option name 'undefined))
  (((match-arg name name help help))
   (arg-option name help))
  ((`#m(name ,name help ,help))
   (arg-option name help))
  ((`#m(name ,name))
   (arg-option name 'undefined)))

(defun arg-option
  ((name 'undefined)
   `#(,(++ "<" name ">") ""))
  ((name help)
   `#(,(++ "<" name ">") ,help)))

(defun wrap-line (text width indent)
  (string:trim
   (string:trim
    (lutil-text:wrap
     (lists:flatten text)
     width
     (* 2 indent))
    'both "\n")
   'leading "  "))