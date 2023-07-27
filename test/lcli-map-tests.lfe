(defmodule lcli-map-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lcli/include/records.lfe")

(deftest app-map->record
  (let* ((m #m(type app name "my-app"))
         (r (lcli-map:->record m)))
    (is-equal 'true (is_record r 'app))
    (is-equal "my-app" (app-name r))))

(deftest group-map->record
  (let* ((m #m(type group summary "the X group"))
         (r (lcli-map:->record m)))
    (is-equal 'true (is_record r 'group))
    (is-equal "the X group" (group-summary r))))

(deftest command-map->record
  (let* ((m #m(type command title "the X command"))
         (r (lcli-map:->record m)))
    (is-equal 'true (is_record r 'command))
    (is-equal "the X command" (command-title r))))

(deftest option-map->record
  (let* ((m #m(type option long "ex" short #\x help "the X option"))
         (r (lcli-map:->record m)))
    (is-equal 'true (is_record r 'option))
    (is-equal "the X option" (option-help r))))

(deftest arg-map->record
  (let* ((m #m(type arg name "anarg"))
         (r (lcli-map:->record m)))
    (is-equal 'true (is_record r 'arg))
    (is-equal "anarg" (arg-name r))))

(deftest help-map->record
  (let* ((m #m(type help title "ex"))
         (r (lcli-map:->record m)))
    (is-equal 'true (is_record r 'help))
    (is-equal "NAME" (help-title-heading r))
    (is-equal "ex" (help-title r))))

(deftest parsed-map->record
  (let* ((m #m(type parsed app "./script.lfe"))
         (r (lcli-map:->record m)))
    (is-equal 'true (is_record r 'parsed))
    (is-equal "./script.lfe" (parsed-app r))))
