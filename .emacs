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
;;  M-<TAB>(C-M-i) : complete the word at point by local mode.
;;  C-s : forward incremental search
;;  C-s C-w : forward search the word under the cursor
;;  M-. : jump to tag specified in TAGS
;;  C-u M-. : jump to next tag
;;  C-x C-q : toggle read-only
;;  C-M-d : goto the inner balanced expression
;;  C-M-u : goto the outer balanced expression

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
(show-paren-mode 1)
(setq show-paren-delay 0  
      show-paren-style 'expression) ; highlight the expression between parenthesis

;;; This is the binary name of my scheme implementation  
; (setq scheme-program-name "~/bin/scheme")

;;; cc-mode
(require 'cc-mode)

;;; CEDET
(load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
;; Semantic
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
(global-semantic-highlight-func-mode 1)
(global-semantic-idle-local-symbol-highlight-mode 1)
(if (fboundp #'which-func-mode)
    (add-hook 'semantic-init-hook (lambda ()
				    (which-func-mode 1))))
(semantic-load-enable-semantic-debugging-helpers)
;; add user defined include dirs for C++
(require 'semantic-c nil 'noerror)
(defconst cedet-linux-system-include-dirs
  (list "/usr/local/include"))
(defconst cedet-user-include-dirs
  (list ".." "../include" "../.." "include"))
(mapc (lambda (dir)
	(semantic-add-system-include dir 'c++-mode))
      cedet-linux-system-include-dirs)
(mapc (lambda (dir)
	(semantic-add-system-include dir 'c++-mode))
      cedet-user-include-dirs)

;;; Org-mode setup
(setq load-path (cons "~/.emacs.d/org-7.8.03/lisp" load-path))
(setq load-path (cons "~/.emacs.d/org-7.8.03/contrib" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;; Auto-complete 
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(define-key ac-completing-map [return] 'ac-complete)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(setq ac-auto-show-menu 0.2)

;;; Yasnippet
(add-to-list 'load-path
              "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/global-mode 1)
;; auto-complete configuration for Yasnippet
(setq-default ac-sources (cons 'ac-source-yasnippet
		       ac-sources))
(setq yas/prompt-functions '(yas/dropdown-prompt))
;; Auto-complete for semantic
(add-hook 'c-mode-common-hook (lambda ()
				(setq ac-sources
				      (cons 'ac-source-semantic
					    ac-sources))))

;;; Tabbar mode
(require 'tabbar)
(tabbar-mode)
(tabbar-mwheel-mode)

;;; Protobuf mode
(require 'protobuf-mode)

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
(global-set-key (kbd "C-x C-S-o") 'other-frame)
(global-set-key (kbd "C-S-k") (lambda ()
				(interactive)
				(kill-buffer (current-buffer))))
(global-set-key (kbd "C-S-j") (lambda ()
				(interactive)
				(move-end-of-line nil)
				(newline-and-indent)))

(global-set-key '[f5] 'compile)
(global-set-key '[f6] 'speedbar)

;; change font size
(if (eq system-type 'gnu/linux)
    ; linux 
    (progn
      (global-set-key '[C-mouse-4] 'text-scale-increase)
      (global-set-key '[C-mouse-5] 'text-scale-decrease))
  ; windows 
  (progn
    (global-set-key '[C-wheel-up] 'text-scale-increase)
    (global-set-key '[C-wheel-down] 'text-scale-decrease)))

(global-set-key (kbd "C-.") '(lambda ()
			       (interactive)
			       (find-tag "" t)))

(global-set-key (kbd "C-{") 'last-blank-block)
(global-set-key (kbd "C-}") 'next-blank-block)

(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "M-n") 'next-buffer)

;; Semantic
; "C-]" is originally bound to abort-recursive-edit
(define-key c-mode-base-map (kbd "C-]") 'semantic-ia-fast-jump)
(define-key c-mode-base-map (kbd "C-t") 'semantic-mrub-switch-tags)

;; hippie-expand config
(global-set-key (kbd "M-/") 'hippie-expand)

;; auto-complete
(global-set-key [(control tab)] 'auto-complete)

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
