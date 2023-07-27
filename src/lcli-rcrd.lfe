(defmodule lcli-rcrd
  (export
   (->map 1))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "lcli/include/records.lfe")

(defun ->map
  (((match-app name n title t desc d options os args as commands cs lib-opts los))
   `#m(type app
       name ,n
       title ,t
       desc ,d
       options ,os
       args ,as
       commands ,cs
       lib-opts ,los))
  (((match-group name n summary s order o))
   `#m(type group
       name ,n
       summary ,s
       order ,o))
  (((match-command name n title t desc d subcommands ss options os args as order o groups gs runner r))
   `#m(type command
       name ,n
       title ,t
       desc ,d
       subcommands ,ss
       options ,os
       args ,as
       order ,o
       groups ,gs
       runner ,r))
  (((match-option name n short s long l help h val-type vt default d required r multi-valued mv delim dl value-parser vp env e order o groups gs))
   `#m(type option
       name ,n
       short ,s
       long ,l
       help ,h
       val-type ,vt
       default ,d
       required ,r
       multi-valued ,mv
       delim ,dl
       value-parser ,vp
       env ,e
       order ,o
       groups ,gs))
  (((match-arg name n required r help h))
   `#m(type arg
       name ,n
       required ,r
       help ,h))
  (((match-help title-heading th title t synopsis-heading sh synopsis s desc-heading dh desc d options-heading oh options os commands-heading ch commands cs additional a))
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
       commands ,cs
       options ,os
       args ,as)))
