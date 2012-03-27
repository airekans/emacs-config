;;; Some useful key bindings for buffer navigation
;;  C-g g : goto-line
;;  C-x C-v : open a alternate file
;;  C-x C-w : save as another file
;;  M-{ : goto the previous blank line
;;  M-} : goto the next blank line
;;  M-< : goto the beginning of the buffer
;;  M-> : goto the end of the buffer
;;  M-^ : join the current line to the previous line(J in Vi).
;;  M-m : move to the first non-blank character on the current line.
;;  C-c < : shift region left
;;  C-c > : shift region right

;;; key bindings for elisp
;;  C-x C-e : evaluate the last elisp expression and print to minibuf
;;  C-j : same as C-x C-e but print the result to the buffer
;;  C-M-a : previous expression
;;  C-M-e : next expression
;;  C-M-p : previous matching parenthesis
;;  C-M-n ; next matching parenthesis

;;; Useful interactive functions
;;  customize-group -> indent : set the indentation settings

;;; Add the .emacs.d to the load-path
(add-to-list 'load-path '"~/.emacs.d")

;;; set the color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-robin-hood)

;;; Always do syntax highlighting  
(global-font-lock-mode 1)  
;;; Also highlight parens  
(setq show-paren-delay 0  
      show-paren-style 'parenthesis)  
(show-paren-mode 1)  
;; highlight the expression between parenthesis
(setq show-paren-style 'expression)

;;; This is the binary name of my scheme implementation  
; (setq scheme-program-name "~/bin/scheme")

;;; Org-mode setup
(setq load-path (cons "~/.emacs.d/org-7.8.03/lisp" load-path))
(setq load-path (cons "~/.emacs.d/org-7.8.03/contrib" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;; local function definitions and keymaps
(defun last-blank-block ()
  (interactive)
  (if (search-backward-regexp "^[ \t]*[^ \t\n]+.*\n\\([ \t]*\n\\)+"
			      (point-min)
			      1)
      (next-line)))
(defun next-blank-block ()
  (interactive)
  (if (search-forward-regexp "\\(\n[ \t]*\\)+\n[ \t]*[^ \t\n]+"
			     (point-max)
			     1)
      (move-beginning-of-line 0)
    (move-beginning-of-line nil)))

;;; key bindings
(global-set-key (kbd "C-S-o") 'other-window)

(global-set-key (kbd "C-{") 'last-blank-block)
(global-set-key (kbd "C-}") 'next-blank-block)

(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "M-n") 'next-buffer)

;; hippie-expand config
(global-set-key (kbd "M-/") 'hippie-expand)

(global-linum-mode 1)


;;; autopair mode
(require 'autopair)
(autopair-global-mode)

;;; pdb setup, note the python version
;; change the following path to the pdb path on your system
;; on Windows, "python -u" must be added before the path to pdb
;; on Linux, you can directly specify the path to pdb
(setq pdb-path 'python\ -u\ d:/python25_link/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
			    ;;  use the following line if you are using
			    ;;  Linux
			    ;; (file-name-nondirectory buffer-file-name)
	 		    (buffer-file-name)))))
