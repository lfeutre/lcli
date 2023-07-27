(defmodule lcli-rcrd-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lcli/include/records.lfe")

(deftest app-rcrd->map
  (let* ((r (make-app name "my-app"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'app (mref m 'type))
    (is-equal "my-app" (mref m 'name))))

(deftest group-rcrd->map
  (let* ((r (make-group summary "the X group"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'group (mref m 'type))
    (is-equal "the X group" (mref m 'summary))))
