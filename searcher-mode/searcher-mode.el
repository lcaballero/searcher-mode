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
		  (modify-syntax-entry i "w" st))))
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

(defun xplor-searcher (q)
  (interactive "Mquery:")
  (message "type of q: %s" (type-of q)))

(defvar searcher-query "")

(defun searcher-run-query (q)
  "Makes an http request using the value provided at the prompt."
  (interactive "Mquery: ")
  (unhighlight-regexp searcher-query)
  (setq searcher-query q)
  (message "type-of q: %s" (type-of searcher-query))
  (message "current query: %s %s" searcher-query q)
  (with-current-buffer (pop-to-buffer "request demo")
	(request
	 (format "http://127.0.0.1:4000/searching?show-query=on&q=%s" q)
	 :parser 'buffer-string
	 :success (function*
			   (lambda (&key data &allow-other-keys)
				 (when data
				   (message "received data")
				   (setq buffer-read-only nil)
				   (message "erasing buffer")
				   (erase-buffer)
				   (message "inserting data %s" (symbol-name (type-of data)))
				   (insert data)
				   (insert (format "Highlighting: %s" searcher-query))
				   (highlight-regexp (format "%s" searcher-query) font-lock-type-face)
				   (message "previous-query %s" searcher-query)
				   (setq buffer-read-only t)
				   (goto-char (point-min))
				   (message "max-point: %d" (point-max))
				   )))
	 )))

(defvar searcher-mode-map
  (let ((m (make-keymap)))
;	(define-key m (kbd "TAB") 'searcher-toggle-current-line)
	(define-key m (kbd "<C-tab>") 'searcher-toggle-all-lines)
	(define-key m (kbd "C-c C-t") 'searcher-toggle-all-lines)
	(define-key m (kbd "C-c t") 'searcher-toggle-all-lines)
	(define-key m (kbd "F") 'searcher-toggle-all-lines)
	(define-key m (kbd "n") 'next-line)
	(define-key m (kbd "p") 'previous-line)
	(define-key m (kbd "a") 'move-beginning-of-line)
	(define-key m (kbd "A") 'beginning-of-buffer)
	(define-key m (kbd "e") 'move-end-of-line)
	(define-key m (kbd "E") 'end-of-buffer)
	(define-key m (kbd "q") 'delete-window)
	(define-key m (kbd "S") 'searcher-run-query)
	(define-key m (kbd "C-c C-f") 'searcher-run-query)
	(define-key m (kbd "C-c f") 'searcher-run-query)
	m))

(setq searcher-font-lock-patterns
  '(("^ +[0-9]+ "     . font-lock-function-name-face)
	("^in .*$" . font-lock-comment-face)
	("[0-9]+: ?[0-9]+ |" . font-lock-variable-name-face)

	("^warning .*?" . font-lock-warning-face)
	("^name .*?" . font-lock-function-name-face)
	("^variable .*?" . font-lock-variable-name-face)
	("^keyword .*?" . font-lock-keyword-face)
	("^comment .*?" . font-lock-comment-face)
	("^comment_delimiter .*?" . font-lock-comment-delimiter-face)
	("^type .*?" . font-lock-type-face)
	("^constant .*?" . font-lock-constant-face)
	("^builtin .*?" . font-lock-builtin-face)
	("^preprocessor .*?" . font-lock-preprocessor-face)
	("^string .*?" . font-lock-string-face)
	("^doc .*?" . font-lock-doc-face)
	("^negation-char .*?" . font-lock-negation-char-face)
 	)
)


; https://www.emacswiki.org/emacs/ModeTutorial
(define-derived-mode searcher-mode nil "searcher"
  :syntax-table searcher-syntax-table
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq show-trailing-whitespace nil)
  (setq font-lock-defaults '(searcher-font-lock-patterns)))

(defun searcher-set-mode-for-special-file (buffer alist)
  (interactive)
  (if (string= (buffer-name buffer) "request demo")
	  (with-current-buffer buffer
		(searcher-mode)
		)))

(put 'searcher-mode 'mode-class 'special)

(add-to-list 'display-buffer-alist '(".*" . (searcher-set-mode-for-special-file)))

(provide 'searcher-mode)

;;; searcher-mode.el ends here
