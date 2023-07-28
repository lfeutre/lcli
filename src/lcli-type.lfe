(defmodule lcli-type
  ;; predicates
  (export
   (maplist? 1)
   (recordlist? 2)
   (speclist? 1)
   (tuplelist? 1))
  ;; converters
  (export
   (maps->records 1)
   (map->record 1)
   (record->map 1)
   (records->maps 1))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "lcli/include/records.lfe")

;;; Predicates

(defun maplist?
  (('())
   'false)
  ((l) (when (is_list l))
   (lists:all #'is_map/1 l))
  ((_)
   'false))

(defun recordlist?
  (('() record-type)
   'false)
  ((l record-type) (when (is_list l))
   (lists:all (lambda (x) (is_record x record-type)) l))
  ((_ _)
   'false))

(defun speclist?
  (('())
   'false)
  ((l) (when (is_list l))
   (lists:all #'spec-item?/1 l))
  ((_)
   'false))

(defun tuplelist?
  (('())
   'false)
  ((l) (when (is_list l))
   (lists:all #'is_tuple/1 l))
  ((_)
   'false))

(defun spec-item?
  ((data) (when (or (is_tuple data) (is_atom data)))
   'true)
  ((_)
   'false))

;;; Converters

(defun records->maps (records)
  (lists:map #'record->map/1 records))

(defun record->map
  ((input) (when (is_map input))
   input)
  (((match-app name n title t desc d options os args as commands cs lib-opts los))
   `#m(type app
       name ,n
       title ,t
       desc ,d
       options ,(records->maps os)
       args ,as
       commands ,(records->maps cs)
       lib-opts ,(records->maps los)))
  (((match-group name n summary s order o))
   `#m(type group
       name ,n
       summary ,s
       order ,o))
  (((match-command name n title t desc d subcommands ss options os args as
                   order o groups gs runner r))
   `#m(type command
       name ,n
       title ,t
       desc ,d
       subcommands ,(records->maps ss)
       options ,(records->maps os)
       args ,(records->maps as)
       order ,o
       groups ,(records->maps gs)
       runner ,r))
  (((match-option name n short s long l help h val-type vt default d
                  required? r multi-valued? mv delim dl value-parser vp
                  env e order o groups gs))
   `#m(type option
       name ,n
       short ,s
       long ,l
       help ,h
       val-type ,vt
       default ,d
       required? ,r
       multi-valued? ,mv
       delim ,dl
       value-parser ,vp
       env ,e
       order ,o
       groups ,(records->maps gs)))
  (((match-arg name n required? r help h))
   `#m(type arg
       name ,n
       required? ,r
       help ,h))
  (((match-help title-heading th title t synopsis-heading sh synopsis s
                desc-heading dh desc d options-heading oh options os
                commands-heading ch commands cs additional a))
   `#m(type help
       title-heading ,th
       title ,t
       synopsis-heading ,sh
       synopsis ,s
       desc-heading ,dh
       desc ,d
       options-heading ,oh
       options ,os
       commands-heading ,ch
       commands ,cs
       additional ,a))
  (((match-parsed app a commands cs options os args as))
   `#m(type parsed
       app ,a
       commands ,(records->maps cs)
       options ,(records->maps os)
       args ,(records->maps as))))

(defun maps->records (maps)
  (lists:map #'map->record/1 maps))

(defun map->record
  ((input) (when (== (is_map input) 'false))
   input)
  (((= `#m(type app) m))
   (make-app name (maps:get 'name m "app")
             title (maps:get 'title m "")
             desc (maps:get 'desc m "")
             options (maps->records (maps:get 'options m '()))
             args (maps->records (maps:get 'args m '()))
             commands (maps->records (maps:get 'commands m '()))
             lib-opts (maps->records (maps:get 'lib-opts m '()))))
  (((= `#m(type group) m))
   (make-group name (maps:get 'name m 'undefined)
               summary (maps:get 'summary m "")
               order (maps:get 'order m 0)))
  (((= `#m(type command) m))
   (make-command name (maps:get 'name m "")
                 title (maps:get 'title m "")
                 desc (maps:get 'desc m "")
                 subcommands (maps->records (maps:get 'subcommands m '()))
                 options (maps->records (maps:get 'options m '()))
                 args (maps->records (maps:get 'args m '()))
                 order (maps:get 'order m 0)
                 groups (maps->records (maps:get 'groups m '()))
                 runner (maps:get 'runner m '())))
  (((= `#m(type option) m))
   (make-option name (maps:get 'name m 'undefined)
                short (maps:get 'short m 'undefined)
                long (maps:get 'long m 'undefined)
                help (maps:get 'help m 'undefined)
                val-type (maps:get 'val-type m 'undefined)
                default (maps:get 'default m 'undefined)
                required? (maps:get 'required? m 'false)
                multi-valued? (maps:get 'multi-valued? m 'false)
                delim (maps:get 'delim m ",")
                value-parser (maps:get 'value-parser m 'undefined)
                env (maps:get 'env m "")
                order (maps:get 'order m 0)
                groups (maps->records (maps:get 'groups m '()))))
  (((= `#m(type arg) m))
   (make-arg name (maps:get 'name m "")
             required? (maps:get 'required? m 'false)
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
                 commands (maps->records (maps:get 'commands m '()))
                 options (maps->records (maps:get 'options m '()))
                 args (maps->records (maps:get 'args m '()))))
  ((unmatched)
   (lfe_io:format "#'map->record/1 could not match ~p~n" (list unmatched))))
