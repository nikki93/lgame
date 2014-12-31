(in-package #:lgame)

;;
;; macros
;;

(defmacro do-hash ((k v table) &body body)
  "Look through entries of hash table `table', binding `k' to the key and `v'
to the value"
  `(maphash #'(lambda (,k ,v) ,@body) ,table))


