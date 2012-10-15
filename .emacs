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
;;  C-\ : toggle input method
;;  M-\ : join two words. useful to deleting white space.
;;  M-^ : join two lines. Same as 'J' in vim.
;;  M-! : run sync shell command in minibuffer
;;  M-| : run the shell command and pass the region as the stdin to the command.

;;; key bindings for elisp
;;  C-x C-e : evaluate the last elisp expression and print to minibuf
;;  C-j : same as C-x C-e but print the result to the buffer
;;  C-M-a : previous expression
;;  C-M-e : next expression
;;  C-M-p : previous matching parenthesis
;;  C-M-n ; next matching parenthesis

;;; Useful interactive functions
;;  customize-group -> indent : set the indentation settings
;;  occur -> find the lines matching the regex
;;  find-dired -> use "find" cmd to find files and show the result in dired mode

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

;;; Set default font
;(set-face-attribute 'default nil :font "Source Code Pro" :height 140)

;;; Linum mode
(global-linum-mode 1)
;;; Column number mode
(column-number-mode)

;;; ido-mode for buffer/file switching
(ido-mode t)
(setq ido-enable-flex-matching t)
(ido-everywhere)
(setq ido-use-filename-at-point 'guess)

;;; This is the binary name of my scheme implementation  
; (setq scheme-program-name "~/bin/scheme")

;; font-lock for scheme mode
(add-hook 'scheme-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("(\\(\\<\\sw+\\?\\)" 1
                                       font-lock-function-name-face append)
                                      ("(\\(\\<\\sw+!\\)" 1
                                       font-lock-warning-face append))
                                    'append))
          'append)

;;; cc-mode
(require 'cc-mode)
(add-hook 'c-mode-common-hook (lambda ()
				(c-toggle-hungry-state 1)))

;; c/c++-mode indent style
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "k&r")))
(add-hook 'c++-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")))
 
(setq c-basic-offset 4)

;; font-lock for cc-mode
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\):" 1
				       font-lock-warning-face t)
				      ("\\<\\(\\sw+\\)\\s-*\\(\\.\\|->\\)" 1
				       font-lock-variable-name-face keep)
				      ("\\<\\(m_\\|d_\\)\\sw+" . font-lock-variable-name-face)
				      ("\\<\\(\\sw+\\)\\s-*(" 1
				       font-lock-function-name-face append))
				    'append))
	  'append)

;;; CEDET
(load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
;; Semantic
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
(global-semantic-highlight-func-mode 1)
(global-semantic-idle-local-symbol-highlight-mode 1)
(global-semantic-decoration-mode 1)
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
(tabbar-mwheel-mode 1)

;;; Protobuf mode
(require 'protobuf-mode)

;;; Pymacs
(setenv "PYTHONPATH" (concat (getenv "HOME")
			     "/.emacs.d/pymacs:"
			     (getenv "PYTHONPATH")))
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;;; ropemacs
;; Before running these, ensure that you have properly installed
;; rope, ropemacs
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;;; autopair mode
(require 'autopair)
(autopair-global-mode)

;;; New python.el


;;; Python mode
;(require 'python-mode)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;; IPython mode
;(require 'ipython)

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

(defun elisp-push-point-marker ()
  "Push point to find-tag-marker-ring. Use M-* to jump back."
  (require 'etags)
  (cond ((featurep 'xemacs)
         (push-tag-mark))
        (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
  (t (message "No symbol at point"))))


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

(define-key emacs-lisp-mode-map (kbd "C-]") 'elisp-find-definition)

;; C-M-\ was bound to indent-region
(global-set-key (kbd "C-M-\\") 'comment-region)
;; C-M-/ was bound to dabbrev-completion
(global-set-key (kbd "C-M-/") 'uncomment-region)

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
; "C-t" is originally bound to transpose-chars
(define-key c-mode-base-map (kbd "C-t") 'semantic-mrub-switch-tags)
(define-key c-mode-base-map (kbd "C-=") 'semantic-analyze-proto-impl-toggle)
(global-set-key (kbd "C-M-m") 'eassist-list-methods)

;; hippie-expand config
(global-set-key (kbd "M-/") 'hippie-expand)

;; auto-complete
(global-set-key [(control tab)] 'auto-complete)

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


;;; Debug options
;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)
;; (add-hook 'after-init-hook
;;          (lambda () (progn (setq debug-on-error t))))


;;; Semantic customization
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(semantic-idle-scheduler-idle-time 0.5))

