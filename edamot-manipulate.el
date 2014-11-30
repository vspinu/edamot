;; manipulate nrepl-dict objects representing tabular R data

(require 'dash)

(defun nrepl-dict-get-in (dict keys)
  (if keys
      (nrepl-dict-get-in
       (nrepl-dict-get dict (car keys))
       (cdr keys))
    dict))

(defmacro dget (dict &rest keys)
  `(nrepl-dict-get-in ,dict (list ,@keys)))

(defmacro dput (dict key value)
  `(nrepl-dict-put ,dict ,key ,value))

(defmacro dmap (fn dict)
  `(nrepl-dict-map ,fn ,dict))

(defmacro zmap (fn dict)
  `(-interleave (nrepl-dict-keys ,dict)
		(nrepl-dict-map ,fn ,dict)))

(defmacro zmap! (fn dict)
  `(-interleave (nrepl-dict-keys ,dict)
		(nrepl-dict-map ,fn ,dict)))

(defun edamot-add-attributes (edadict)
  "Add additional information to EDADICT's columns.
Modifies EDADICT in place"
  (nrepl-dict-map  (lambda (cn col)
		     (let ((w (-reduce 'max (-map 'length (cons cn (dget col "data"))))))
		       (dput (dget col "info") :col-name cn)
		       (dput (dget col "info") :col-width w)
		       ;; (dput col :hidden? nil)
		       nil))
		   (dget edadict "data"))
  edadict)



