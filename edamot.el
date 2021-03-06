
(require 'nrepl-client)

(defgroup edamot nil
  "Emacs DAta MOdeling Toolkit."
  :prefix "edamot-")

(defcustom edamot-R-command "R"
  "Path to R's executable.")

(defcustom edamot-R-params "-q -e \"library(nREPL); start_server()\""
  "Arguments to `edamot-R-command'.")

(defcustom cider-connected-hook nil
  "List of functions to call when connected to Clojure nREPL server."
  :type 'hook
  :group 'cider)

(defcustom cider-disconnected-hook nil
  "List of functions to call when disconnected from the Clojure nREPL server."
  :type 'hook
  :group 'cider)

(defvar edamot-host-history '()
  "Holds history of host completions")

;;;###autoload
(define-derived-mode edamot-mode special-mode "Edamot"
  "Interactive data modeling with R."
  (setq edamot--cursor-overlay (make-overlay 1 1))
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local cursor-type nil)
  ;; (add-hook 'post-command-hook 'edamot--post-command-handler nil t)
  (overlay-put edamot--cursor-overlay 'face 'edamot-cursor-face))

;;;###autoload
(defun edamot-jack-in ()
  "Start a nREPL R server and connect to it."
  (interactive)
  (if (or (file-remote-p default-directory)
	  (executable-find edamot-R-command))
      (let ((nrepl-create-client-buffer-function  #'edamot-create-client-buffer)
	    (cmd (format "%s %s" edamot-R-command edamot-R-params)))
        (when (nrepl-check-for-repl-buffer nil default-directory)
          (nrepl-start-server-process nil cmd)))
    (message "The '%s' executable (specified by `edamot-R-command') is not on your exec-path"
             edamot-lein-command)))

;;;###autoload
(defun edamot-connect (host port)
  (interactive (edamot--select-endpoint))
  (when (nrepl-check-for-repl-buffer `(,host ,port) nil)
    (let ((nrepl-create-client-buffer-function #'edamot-create-client-buffer))
     (nrepl-start-client-process host port))))

(defun edamot--select-endpoint ()
  (let* ((host (completing-read "Host: " (list "localhost")
				nil nil nil 'edamot-host-history "localhost"))
	 (port (completing-read (format "Port for %s: " host) (list "4005")
				nil nil nil nil nil "4005")))
    (list host port)))

(defun edamot-create-client-buffer (endpoint)
  "Create a REPL buffer and install `cider-repl-mode'.
ENDPOINT is a plist as returned by `nrepl-connect'."
  (let* ((nrepl-connection-buffer-name-template "*edamot%s*")
	 (buf (nrepl-make-buffer-name nrepl-repl-buffer-name-template nil
				      (plist-get endpoint :host)
				      (plist-get endpoint :port))))
    (with-current-buffer (get-buffer-create buf)
      (unless (derived-mode-p 'edamot-mode)
        (edamot-mode))
      (add-hook 'nrepl-connected-hook 'edamot--connected-handler nil 'local)
      (add-hook 'nrepl-disconnected-hook 'edamot--disconnected-handler nil 'local))
    (switch-to-buffer buf)
    buf))

(defun edamot--connected-handler ()
  "Handle edamot initialization after nREPL connection has been established.
This function is appended to `nrepl-connected-hook' in the client
process buffer."
  (run-hooks 'edamot-connected-hook))

(defun edamot--disconnected-handler ()
  "Cleanup after nREPL connection has been lost or closed.
This function is appended to `nrepl-disconnected-hook' in the
client process buffer."
  (nrepl--close-connection-buffer (current-buffer))
  (run-hooks 'edamot-disconnected-hook))
