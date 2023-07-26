(defmodule lcli-spec-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun specs-1s ()
  '(#(help #\h "help" undefined "Display help text.")))

(defun specs-2s ()
  '(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")))

(defun specs-3s ()
  '(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")
    #(commands #\c "commands" string "Comma-separated commands.")))

(defun specs-4s ()
  `(#(help #\h "help" undefined "Display help text.")
    #(file #\f "file" string "Use this file.")
    #(commands (#(db #(options (#(help #\h "help" undefined "Display help for 'db' command."))))
                #(app #(options (#(help #\h "help" undefined "Display help for 'app' command."))))))))

(defun specs-1m ()
  '(#m(short #\h long "help" help "Display help text.")))

(defun specs-2m ()
  '(#m(short #\h long "help" help "Display help text.")
    #m(short #\f long "file" type string help "Use this file.")))

(defun specs-3m ()
  '(#m(short #\h long "help" help "Display help text.")
    #m(short #\f long "file" type string help "Use this file.")
    #m(short #\c long "commands" type string help "Comma-separated commands.")))

(defun specs-4m ()
  '(#m(long "help" short #\h help "Display help text.")
    #m(long "file" short #\f type string help "Use this file.")
    #m(commands
       (#m(name "db" options (#m(long "help" short #\h help "Display help for 'db' command.")))
        #m(name "app" options (#m(long "help" short #\h help "Display help for 'app' command.")))))))

(deftest specs->maps-simple
  (is-equal (specs-1m)
            (lcli-spec:->maps (specs-1s)))
  (is-equal (specs-2m)
            (lcli-spec:->maps (specs-2s))))

(deftest specs->maps-non-list-commands
  (is-equal (specs-3m)
            (lcli-spec:->maps (specs-3s))))

(deftest specs->maps-list-commands
  (is-equal (specs-2m)
            (lcli-spec:->maps (specs-4s))))

(deftest specs->maps-with-maps
  (is-equal (specs-1m)
            (lcli-spec:->maps (specs-1m)))
  (is-equal (specs-2m)
            (lcli-spec:->maps (specs-2m)))
  (is-equal (specs-3m)
            (lcli-spec:->maps (specs-3m)))
  (is-equal (specs-2m)
            (lcli-spec:->maps (specs-4m))))

(deftest specs->
  ;; simple specs
  (is-equal (specs-1s)
            (lcli-spec:-> (specs-1s)))
  (is-equal (specs-2s)
            (lcli-spec:-> (specs-2s)))
  ;; non-list 'commands' specs
  (is-equal (specs-3s)
            (lcli-spec:-> (specs-3s)))
  ;; list 'commands' specs
  (is-equal (specs-2s)
            (lcli-spec:-> (specs-4s)))
  ;; maps
  (is-equal (specs-1s)
            (lcli-spec:-> (specs-1m)))
  (is-equal (specs-2s)
            (lcli-spec:-> (specs-2m)))
  (is-equal (specs-3s)
            (lcli-spec:-> (specs-3m)))
  (is-equal (specs-2s)
            (lcli-spec:-> (specs-4m))))