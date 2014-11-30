(load "~/VC/edamot/edamot.el")
(load "~/VC/edamot/edamot-manipulate.el")


(defface edamot-header-face
  '((t (:inherit mode-line :box nil)))
  "Face for displaying the header of the table."
  :group 'edamot)

(defface edamot-footer-face
  '((t (:inherit mode-line :box nil)))
  "Face for displaying the footer of the table."
  :group 'edamot)

(defface edamot-cursor-face
  '((t (:inverse-video t)))
  "Face for displaying Edamot's cursor."
  :group 'edamot)

(defface edamot-horizontal-delimiter-face
  '((t (:inherit default :strike-through t)))
  "Face for horizontally delimiting various sections of the edamot buffers."
  :group 'edamot)

(defcustom edamot-max-column-width 20
  "Maximal width of table columns.")

(defcustom edamot-min-column-width 20
  "Minimal width of table columns.")

(defvar-local edamot-current-edadict nil)
(defvar-local edamot-hiden-columns nil)

(defvar-local edamot--cursor-overlay nil)

(defun edamot--get-cell-at-point (&optional pos)
  (let ((inhibit-point-motion-hooks t)
	(pos (or pos (point))))
    (cons pos (next-single-property-change pos :col-name nil (point-max)))))

(defvar old-command nil)

(defun edamot--point-entered-handler (old new)
  (let ((inhibit-point-motion-hooks t)
	(next-prop (get-char-property new :col-name))
	(prev-prop (get-char-property (max (1- new) (point-min)) :col-name)))
    ;; (when (equal next-prop prev-prop)
    ;;   (if (> new old)
    ;; 	  (goto-char (next-single-property-change new :col-name nil (point-max)))
    ;; 	(goto-char (previous-single-property-change new :col-name nil (point-min)))))
    (when (and (bolp) (eobp))
      (backward-char))
    (when (eolp)
      (if (<= new old)
    	  (goto-char (previous-single-property-change new :col-name nil (point-min)))
    	(unless (eobp)
    	  (forward-line 1))))
    (let ((pos (edamot--get-cell-at-point (min (1+ (point)) (point-max)))))
      (move-overlay edamot--cursor-overlay (car pos) (cdr pos)))))

(defun edamot-body-height ()
  (let ((ww (window-total-height)))
    (- ww 17)))

(defun edamot--forward-line ()
  (if (not (eq (point-at-eol) (point-max)))
      (forward-line 1)
    (goto-char (point-max))
    (insert (propertize "\n"  'point-entered #'edamot--point-entered-handler))))

(defun edamot--insert-col-data (data nr fmt start-col end-col &rest props)
  (let ((data (if (listp data) data (list data))) 
	(nr (or nr (length data))))
   (dotimes (v nr)
     (move-to-column start-col 'force)
     (when end-col
       (delete-region (point) (+ point (- end-col start-col))))
     (insert (concat (propertize " " 'intangible (plist-get props 'intangible)
		     		 'point-entered #'edamot--point-entered-handler
				 ;; 'point-left #'edamot--point-entered-handler
				 :col-name v
				 'field v)
		     (apply #'propertize (format fmt (or (car data) ""))
			    ;; 'point-left #'edamot--point-entered-handler
			    ;; 'point-entered #'edamot--point-entered-handler
			    'field v props)))
     (setq data (cdr data))
     (edamot--forward-line))))

(cl-defun edamot-insert-edacol (edacol &key start end start-col end-col)
  "Insert edacol CNAME at current column."
  (unless (dget edacol "info" :hidden?)
    (let* ((start-col (or start-col (current-column)))
	   (start (or start 0))
	   (cname (dget edacol "info" :col-name))
	   (cwidth (dget edacol "info" :col-width))
	   (fmt (concat " %" (number-to-string cwidth) "s")))
      (goto-char (point-min))
      (move-to-column start-col 'force)

      ;; header
      (edamot--insert-col-data cname 1 fmt start-col end-col
			       'face 'edamot-header-face
			       'intangible cname
			       :span-type :head :col-name cname)

      ;; separator
      (edamot--insert-col-data "" 1 fmt start-col end-col
			       'face 'edamot-horizontal-delimiter-face
			       'intangible cname
			       :span-type :separator :col-name cname)

      ;; body
      (edamot--insert-col-data (nthcdr start (dget edacol "data" )) ; data
			       (if end (- end start) (edamot-body-height)) ;nr
			       fmt start-col end-col
			       'intangible cname
			       :span-type :body :col-name cname)
      
      ;; separator
      (edamot--insert-col-data "" 1 fmt start-col end-col
			       'face 'edamot-horizontal-delimiter-face
			       'intangible cname
			       :span-type :separator :col-name cname)
      
      ;; tail
      (edamot--insert-col-data (nrepl-dict-vals (dget edacol "info"))
			       nil fmt start-col end-col
			       'face 'edamot-footer-face
			       'intangible cname
			       :span-type :footer :col-name cname)
      (end-of-line -1))))

(defun edamot-insert-edadict (edadict &optional start end)
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t))
    (erase-buffer)

    ;; (edamot--insert-col-data (make-list 100 "   ")
    ;; 			     nil "%s" 0 nil
    ;; 			     'face 'highlight
    ;; 			     'intangible 'ofset
    ;; 			     :span-type :offset :col-name 'offset)
    ;; (end-of-line 0)
    (dmap (lambda (cn col)
	    (edamot-insert-edacol col :start start :end end))
	  (dget edadict "data"))
    (goto-char (point-min))))
