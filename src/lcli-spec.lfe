(defmodule lcli-spec
  (export
   (-> 1) (-> 2)
   (->map 1)
   (->maps 1)
   (map-> 1)
   (maps-> 1)
   (new-map 0) (new-map 1) (new-map 5) (new-map 6)
   (record-> 2))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "lcli/include/records.lfe")

(defun new-map ()
  #m(short undefined
     long undefined
     type undefined
     help undefined))

(defun new-map (m)
  (maps:merge (new-map) m))

(defun new-map (short long type default help)
  (new-map (atom_to_list long) short long type default help))

(defun new-map (name short long type default help)
  `#m(name ,name
      short ,short
      long ,long
      type ,type
      default ,default
      help ,help))

(defun ->
  ((data) (when (is_record data 'app))
   (-> data 'app))
  ((data) (when (is_record data 'option))
   (-> data 'option))
  ((data) (when (is_list data))
   (cond ((lcli-util:recordlist? data 'app) (-> data 'app))
         ((lcli-util:recordlist? data 'command) (-> data 'command))
         ((lcli-util:recordlist? data 'option) (-> data 'option))
         ('true (-> data 'undefined))))
  ((data)
   (-> data 'undefined)))

(defun ->
  ;; is it a list?
  ((data type) (when (is_list data))
   (cond ((lcli-util:recordlist? data type)
          (clean-specs (lists:map (lambda (x) (record-> x type)) data)))
         ((lcli-util:speclist? data)
          (clean-specs data))
         ((lcli-util:maplist? data)
          (clean-specs (lists:map #'map->/1 data)))
         ('true
          (lfe_io:format "Didn't match list: ~p" (list data)))))
  ;; is it an app record?
  ((data 'app) (when (is_record data 'app))
   (record-> data 'app))
  ;; is it an command record?
  ((data 'command) (when (is_record data 'command))
   (record-> data 'command))
  ;; is it an option record?
  ((data 'option) (when (is_record data 'option))
   (record-> data 'option))
  ;; is the data already a spec?
  ((data _) (when (or (is_tuple data) (is_atom data)))
   data)
  ((data _) (when (and (is_map data) (== (map_size data) 0)))
   '())
  ((data _) (when (is_map data))
   (map-> data)))

(defun maps->
  (('())
   '())
  ((data)
   (if (lcli-util:speclist? data)
     (clean-specs data)
     (lists:map #'map->/1 data))))

(defun map->
  ;; is the data already a spec?
  ((data) (when (or (is_tuple data) (is_atom data)))
   data)
  ((data) (when (== (map_size data) 0))
   '())
  ((`#m(commands ,_))
   '())
  (((= `#m(name ,name long ,long)  map-spec))
   (let ((`#m(name ,name short ,short long ,long help ,help)
             (new-map map-spec)))
     `#(,name ,short ,long ,(map->arg-spec map-spec) ,help)))
  (((= `#m(long ,long) map-spec))
   (map-> (maps:merge map-spec `#m(name ,(list_to_atom long)))))
  (((= `#m(name ,name) map-spec))
   (map-> (new-map map-spec)))
  (((= `#m(short ,short arg ,arg help ,help) args))
   (lcli-error:invalid-spec "one of name or long is required")))

(defun ->maps
  (('())
   '())
  ((specs)
   (lists:filter
    (lambda (x) (=/= (map_size x) 0))
    (lists:map #'->map/1 specs))))

(defun ->map
  (('())
   #m())
  ((data) (when (is_map data))
   (if (andalso (maps:is_key 'commands data)
                (is_list (mref data 'commands)))
     #m()
     data))
  ((`#(,name ,short ,long ,arg-spec ,help))
   (->map name short long arg-spec help))
  ((`#(commands ,vals)) (when (is_list vals))
   #m())
  ((`#(commands ,vals))
   `#m(commands ,vals)))

(defun record->
  (((match-option name n short s long l type t default d help h) 'option)
   (map-> (clean-map `#m(name ,n short ,s long ,l type ,t default ,d help ,h))))
  (((match-command options os) 'command)
   (-> os))
  (((match-app options os) 'app)
   (-> os)))

;;; Private functions

(defun clean-map (map-data)
  (maps:from_list
   (lists:foldl #'clean-map/2
                '()
                (maps:to_list map-data))))

(defun clean-map
  ((`#(,_ undefined) acc)
   acc)
  ((`#(short 0) acc)
   acc)
  ((`#(long "") acc)
   acc)
  ((kv acc)
   (lists:append acc (list kv))))

(defun clean-specs (speclist)
  (lists:foldl #'clean-spec/2
               '()
               speclist))

(defun clean-spec
  (('() acc)
   acc)
  ((`#(commands ,_) acc)
   acc)
  ((spec acc)
   (lists:append acc (list spec))))
   
(defun ->map (name short long arg-spec help)
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
     (lambda (k v) (if (orelse (== v 'undefined)
                               (== v 'unset)
                               (and (== k 'name)
                                    (maps:is_key 'long result)))
                     'false
                     'true))
     result)))

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

(defun map->arg-spec
  ((`#m(type ,arg-type default unset))
   arg-type)
  ((`#m(type ,arg-type default ,default-val))
   `#(,arg-type ,default-val))
  ((`#m(type ,arg-type))
   arg-type)
  ((_)
   'undefined))
