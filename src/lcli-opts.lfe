(defmodule lcli-opts
  (export all))

(defun parse
  "Parse the options in a given spec by calling `getopt:parse`. Returns a
  map of parsed options and trailing args.

  Note that this function will fail if the passed 'specs' contains
  non-option data such as 'commands'."
  ((specs raw-args)
   (case (getopt:parse (maybe-maps-> specs) raw-args)
     (`#(ok #(,opts ,args))
      `#m(opts ,(opts->map opts)
          args ,args))
     (err
      (lcli-error:opt-parse err)))))

(defun maybe-maps->
  (((= `(,spec1 . ,_) specs)) (when (is_map spec1))
   (maps->specs specs))
  ((specs)
   specs))

(defun opt->tuple
  ((opt) (when (is_tuple opt))
   opt)
  ((opt) (when (is_atom opt))
   `#(,opt true)))

(defun opts->map (opts)
  (maps:from_list
   (lists:map
    #'opt->tuple/1
    opts)))

(defun get (key parsed)
  "Extract the value of an option associated with the given key.

  Note that the indended use for this function is for extracting values from
  options that have already been parsed using the provided specs and arguments
  passed via the command line. This this function is essentially a permissive
  version of ``proplists:get_value`` it may also be used for other data."
  (maps:get key (mref parsed 'opts) 'undefined))

(defun args (parsed)
  (mref parsed 'args))

(defun help? (parsed)
  "Test for the presence of the 'help' option (boolean)."
  (if (== (get 'help parsed) 'undefined)
    'false
    'true))

(defun filter-specs (specs)
  "In a list of specs, return only those that are true option specs, not those
  that are lcli-specific 'commands specs'."
  (lists:filtermap #'lcli-cmds:not-commands?/1 specs))

(defun specs->maps (specs)
  (lists:map
   #'spec->map/1
   specs))

(defun spec->map
  ((`#(,name ,short ,long ,arg-spec ,help))
   (spec->map name short long arg-spec help)))

(defun spec->map (name short long arg-spec help)
  "Convert an Erlang getopt spec (`type option_spec`) to a map."
  (let* ((val (arg-spec->map arg-spec))
         (type (maps:get 'type val 'undefined))
         (default (maps:get 'default val 'undefined))
         (result `#m(name ,name
                     short ,short
                     long ,long
                     type ,type
                     default ,default
                     help ,help)))
    (maps:filter
     (lambda (k v) (if (== v 'undefined) 'false 'true))
     result)))

(defun arg-spec->map ()
  #m())

(defun arg-spec->map
  (('())
   #m())
  (('undefined)
   #m())
  ((`#(,arg-type ,default-val))
   `#m(type ,arg-type
       default ,default-val))
  ((arg-type)
   `#m(type ,arg-type
       default unset)))

(defun default-vals
  (('atom) 'undefined)
  (('binary) #"")
  (('utf8_binary) #"")
  (('boolean) 'false)
  (('float) 0.0)
  (('integer) 0)
  (('string) ""))

(defun maps->specs (maps)
  (lists:map
   #'map->spec/1
   maps))

(defun default-map-spec ()
  #m(short undefined
     long undefined
     arg #m()
     help undefined))

(defun map->spec
  (((= `#m(name ,name long ,long)  map-spec))
   (let ((`#m(name ,name short ,short long ,long help ,help)
             (maps:merge (default-map-spec) map-spec)))
     `#(,name ,short ,long ,(map->arg-spec map-spec) ,help)))
  (((= `#m(long ,long) map-spec))
   (map->spec (maps:merge map-spec `#m(name ,(list_to_atom long)))))
  (((= `#m(name ,name) map-spec))
   (map->spec (maps:merge (default-map-spec) map-spec)))
  (((= `#m(short ,short arg ,arg help ,help) args))
   (lcli-error:invalid-spec "one of name or long is required")))
   
(defun map->arg-spec
  ((`#m(type ,arg-type default unset))
   arg-type)
  ((`#m(type ,arg-type default ,default-val))
   `#(,arg-type ,default-val))
  ((`#m(type ,arg-type))
   arg-type)
  ((_)
   'undefined))
