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
       order ,o)))
