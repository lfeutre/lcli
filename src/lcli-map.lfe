(defmodule lcli-map
  (export
   (->record 1))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "lcli/include/records.lfe")

(defun ->record
  (((= `#m(type app) m))
   (make-app name (maps:get 'name m "app")
             title (maps:get 'title m "")
             desc (maps:get 'desc m "")
             options (maps:get 'options m '())
             args (maps:get 'args m '())
             commands (maps:get 'commands m '())
             lib-opts (maps:get 'lib-opts m '())))
  (((= `#m(type group) m))
   (make-group name (maps:get 'name m 'undefined)
               summary (maps:get 'summary m "")
               order (maps:get 'order m 0)))
  (((= `#m(type command) m))
   (make-command name (maps:get 'name m "")
                 title (maps:get 'title m "")
                 desc (maps:get 'desc m "")
                 subcommands (maps:get 'subcommands m '())
                 options (maps:get 'options m '())
                 args (maps:get 'args m '())
                 order (maps:get 'order m 0)
                 groups (maps:get 'groups m '())
                 runner (maps:get 'runner m '())))
  (((= `#m(type option) m))
   (make-option name (maps:get 'name m "")
                short (maps:get 'short m 0)
                long (maps:get 'long m "")
                help (maps:get 'help m "")
                val-type (maps:get 'val-type m 'undefined)
                default (maps:get 'default m 'undefined)
                required (maps:get 'required m 'false)
                multi-valued (maps:get 'multi-valued m 'false)
                delim (maps:get 'delim m ",")
                value-parser (maps:get 'value-parser m 'undefined)
                env (maps:get 'env m "")
                order (maps:get 'order m 0)
                groups (maps:get 'required m 'false)))
  (((= `#m(type arg) m))
   (make-arg name (maps:get 'name m "")
             required (maps:get 'required m 'false)
             help (maps:get 'help m "")))
  (((= `#m(type help) m))
    (make-help title-heading (maps:get 'name m "NAME")
               title (maps:get 'title m "")
               synopsis-heading (maps:get 'name m "SYNOPSIS")
               synopsis (maps:get 'synopsis m "")
               desc-heading (maps:get 'desc-heading m "DESCRIPTION")
               desc (maps:get 'desc m "")
               options-heading (maps:get 'options-heading m "OPTIONS")
               options (maps:get 'options m '())
               commands-heading (maps:get 'commands-heading m "COMMANDS")
               commands (maps:get 'commands m '())
               additional (maps:get 'additional m '())))
  (((= `#m(type parsed) m))
    (make-parsed app (maps:get 'app m "")
                 commands (maps:get 'commands m '())
                 options (maps:get 'options m '())
                 args (maps:get 'args m '()))))
