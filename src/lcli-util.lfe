(defmodule lcli-util
  (export
   (maplist? 1)
   (speclist? 1)
   (tuplelist? 1)))

(defun maplist?
  (('())
   'false)
  ((l) (when (is_list l))
   (lists:all #'is_map/1 l))
  ((_)
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