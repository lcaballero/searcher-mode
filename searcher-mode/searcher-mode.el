;;;; searcher-mode.el -- Major mode for using the walker searcher exe.

(require 'url)
(require 'request)
(require 'cl)

(defvar searcher-syntax-table
  (let ((st (make-syntax-table)))
	(let ((symbol (string-to-syntax "_"))
		  (sst (standard-syntax-table)))
	  (dotimes (i 128)
		(unless (= i ?_)
		  (if (equal symbol (aref sst i))
			  (modify-syntax-entry i "w" st)))))
    (modify-syntax-entry ?+  "w" st)
    (modify-syntax-entry ?-  "w" st)
    (modify-syntax-entry ?%  "w" st)
    (modify-syntax-entry ?&  "w" st)
    (modify-syntax-entry ?|  "w" st)
    (modify-syntax-entry ?^  "w" st)
    (modify-syntax-entry ?!  "w" st)
    (modify-syntax-entry ?=  "w" st)
    (modify-syntax-entry ?<  "w" st)
    (modify-syntax-entry ?>  "w" st)
    (modify-syntax-entry ?/  "w" st)
    (modify-syntax-entry ?*  "w" st)
    (modify-syntax-entry ?\n "w" st)
    (modify-syntax-entry ?\" "w" st)
    (modify-syntax-entry ?\' "w" st)
    (modify-syntax-entry ?`  "w" st)
    (modify-syntax-entry ?\\ "w" st)
    (modify-syntax-entry ?_  "w" st)
	(modify-syntax-entry ?\t "w" st)	
    st)
  "Syntax table for searcher mode.")

(defun searcher-toggle-current-line ()
  (interactive)
  (message "searcher-toggle-current-line"))

(defvar searcher-all-lines-hidden nil)

(defun searcher-toggle-all-lines ()
  (interactive)
  (setq searcher-all-lines-hidden (not searcher-all-lines-hidden))
  (if searcher-all-lines-hidden
	  (hide-lines-matching "^in .*?$")
	(hide-lines-show-all)))


(defun searcher-run-query (q)
  "Makes an http request using the value provided at the prompt."
  (interactive "Mquery: ")
  (with-current-buffer (pop-to-buffer "request demo")
	(message "with-current-buffer")
	(message (buffer-name (current-buffer)))
	(request
	 (format "http://127.0.0.1:4000/searching?q=%s" q)
	 :parser 'buffer-string
	 :error (function*
			 (lambda (&key data &allow-other-keys)
			   (insert data)))
	 :success (function*
			   (lambda (&key data &allow-other-keys)
				 (when data
				   (setq buffer-read-only nil)
				   (erase-buffer)
				   (insert-string data)
				   (setq buffer-read-only t)
				   (goto-char (point-max))
				   ))))))

;(message "%s" (type-of (current-buffer)))
;(searcher-run-query "hope")

(defvar searcher-mode-map
  (let ((m (make-keymap)))
;	(define-key m (kbd "TAB") 'searcher-toggle-current-line)
	(define-key m (kbd "<tab>") 'searcher-toggle-all-lines)
	(define-key m (kbd "n") 'next-line)
	(define-key m (kbd "p") 'previous-line)
	(define-key m (kbd "a") 'move-beginning-of-line)
	(define-key m (kbd "A") 'beginning-of-buffer)
	(define-key m (kbd "e") 'move-end-of-line)
	(define-key m (kbd "E") 'end-of-buffer)
	(define-key m (kbd "C-c C-f") 'searcher-run-query)
	m))

(setq searcher-font-lock-patterns
  '(("^ +[0-9]+ "     . font-lock-function-name-face)
	(" +[0-9]+:[0-9]+ |" . font-lock-constant-face)))

; https://www.emacswiki.org/emacs/ModeTutorial
(define-derived-mode searcher-mode nil "searcher"
  :syntax-table searcher-syntax-table
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq show-trailing-whitespace nil)
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults '(searcher-font-lock-patterns)))

(defun searcher-set-mode-for-special-file (buffer alist)
  (interactive)
  (message "setting searcher mode")
  (if (string= (buffer-name buffer) "request demo")
	  (with-current-buffer buffer
		(searcher-mode))))

(put 'searcher-mode 'mode-class 'special)

(add-to-list 'display-buffer-alist '(".*" . (searcher-set-mode-for-special-file)))

(provide 'searcher-mode)

;;; searcher-mode.el ends here
