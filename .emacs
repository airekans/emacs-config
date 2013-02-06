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
;;  M-& : Async shell command, output to another buffer.
;;  M-| : run the shell command and pass the region as the stdin to the command.
;;  C-x <TAB> : indent-rigidly, which shift the region right with
;;              given prefix argument. 

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
;;  apropos -> find the matching items by using regex.

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

;;; Set scroll-margin so that when moving toward top or bottom,
;;; display will scroll automatically.
(setq scroll-margin 5
      scroll-conservatively 10000)

;;; Set default font
;; To download "Source Code Pro", you can goto http://blogs.adobe.com/typblography/2012/09/source-code-pro.html
;; Then you put all .ttf files under ~/.fonts and run the following commands in ~/.fonts:
;(set-face-attribute 'default nil :font "Source Code Pro" :height 140)

;;; Linum mode
(global-linum-mode 1)
;;; Column number mode
(column-number-mode)

;;;; Convert TAB to space
(set-default 'indent-tabs-mode nil)


;;; ido-mode for buffer/file switching
(ido-mode t)
(setq ido-enable-flex-matching t)
(if (and (= emacs-major-version 23) (= emacs-minor-version 1))
    (ido-everywhere 1)
  (ido-everywhere))
(setq ido-use-filename-at-point 'guess)
(setq ido-max-directory-size nil) ; disable the maximum directory settings.
;; integrate recentf with ido
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 300)

(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))


;;; global-visual-line-mode, which splits a long logical line into several lines
(global-visual-line-mode 1)

;;; Scheme
(require 'xscheme) ;; I use mit-scheme, so the xscheme package.
;;; This is the binary name of my scheme implementation  
; (setq scheme-program-name "~/bin/scheme")

;;; SLIME
(add-to-list 'load-path "~/.emacs.d/slime")  ; SLIME directory
(setq inferior-lisp-program "/usr/local/bin/sbcl") ; Lisp system
(require 'slime)
(slime-setup)

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

;;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

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

;;; paredit mode (for lisp language)
(autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'scheme-mode-hook (lambda ()
			      (paredit-mode +1)
			      (semantic-show-unmatched-syntax-mode -1)))

;;; find-file-in-project
(require 'util)
(if (eq system-type 'gnu/linux)
    (progn (require 'find-file-in-project)
	   (defadvice ffip-project-files (around ffip-files-around-ad activate)
	     (let* ((project-root (or ffip-project-root (ffip-project-root)))
		    (cache-file (expand-file-name ".ffip-cache" project-root)))
	       (if (and (file-exists-p cache-file)
			(< (get-diff-days (current-time)
				       (get-mtime
					(file-attributes cache-file)))
			   1))
		   (setq ad-return-value (car (read-from-string
					       (shell-command-to-string
						(concat "cat " cache-file)))))
		 ad-do-it
		 (when ad-return-value
		   (ignore-errors
		     (with-temp-file cache-file
		       (prin1 ad-return-value (current-buffer))))))))))

;;; Customization for project-local-variables
(require 'project-local-variables)
(defadvice plv-find-project-file (around plv-load-file-around-ad activate)
  (if (boundp 'plv-local-project-file-path)
      (setq ad-return-value plv-local-project-file-path)
    ad-do-it
    (when ad-return-value
      (progn (set (make-local-variable 'plv-local-project-file-path)
		  ad-return-value)
	     (load ad-return-value)))))

;;; Ibus mode
;;; before enable this mode, ensure python-xlib has been installed
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)


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

(defun elisp-goto-definition (name)
  "Go to the definition of the function having the given name"
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

(defun elisp-goto-definition-at-point ()
  "Jump to the definition of the function (or variable) at point."
  (interactive)
  (elisp-goto-definition (symbol-at-point)))

(defun elisp-goto-definition-interactively (name)
;  (interactive "aGoto function: ")
  (interactive (list
		(ido-completing-read
		 "Goto function: "
		 (all-completions "" obarray 'fboundp))))
  (elisp-goto-definition name))


;;; Additional features using ido
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  (symbol-at-pt (symbol-at-point))
	  name-and-pos symbol-names position)
      (unless ido-mode
	(ido-mode 1)
	(setq ido-enable-flex-matching t))
      (while (progn
	       (imenu--cleanup)
	       (setq imenu--index-alist nil)
	       (ido-goto-symbol (imenu--make-index-alist))
	       (setq selected-symbol
		     (ido-completing-read "Symbol? " symbol-names
					  nil nil
					  (if symbol-at-pt
					      (symbol-name symbol-at-pt)
					    "")))
	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
	(cond
	 ((and (listp symbol) (imenu--subalist-p symbol))
	  (ido-goto-symbol symbol))
	 ((listp symbol)
	  (setq name (car symbol))
	  (setq position (cdr symbol)))
	 ((stringp symbol)
	  (setq name symbol)
	  (setq position
		(get-text-property 1 'org-imenu-marker symbol))))
	(unless (or (null position) (null name)
		    (string= (car imenu--rescan-item) name))
	  (add-to-list 'symbol-names name)
	  (add-to-list 'name-and-pos (cons name position))))))))

(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (all-completions "" obarray 'commandp)))))

(defun occur-at-point ()
  "Find occurences of the symbol at point"
  (interactive)
  (require 'hi-lock)
  (let* ((s (symbol-at-point)))
    (if (null s)
	(message "No symbol at point!")
      (let ((name (symbol-name s)))
	(if (not (null hi-lock-interactive-patterns))
	    (unhighlight-regexp (car (car hi-lock-interactive-patterns))))
	(highlight-regexp name)
	(occur name)))))

;;; Smart jumping similar to '*'/'#' in vi
(defvar smart-use-extended-syntax nil
  "If t the smart symbol functionality will consider extended
syntax in finding matches, if such matches exist.")
 
(defvar smart-last-symbol-name ""
  "Contains the current symbol name.
 
This is only refreshed when `last-command' does not contain
either `smart-symbol-go-forward' or `smart-symbol-go-backward'")
 
(make-local-variable 'smart-use-extended-syntax)
 
(defvar smart-symbol-old-pt nil
  "Contains the location of the old point")
 
(defun smart-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.
 
DIRECTION must be either `forward' or `backward'; no other option
is valid."
 
  ;; if `last-command' did not contain
  ;; `smart-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smart-symbol-go-forward
                               smart-symbol-go-backward))
    (setq smart-last-symbol-name name))
  (setq smart-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smart-last-symbol-name))
  (unless (catch 'done
            (while (funcall (cond
                             ((eq direction 'forward) ; forward
                              'search-forward)
                             ((eq direction 'backward) ; backward
                              'search-backward)
                             (t (error "Invalid direction"))) ; all others
                            smart-last-symbol-name nil t)
              (unless (memq (syntax-ppss-context
                             (syntax-ppss (point))) '(string comment))
                (throw 'done t))))
    (goto-char smart-symbol-old-pt)))
 
(defun smart-symbol-go-forward ()
  "Jumps forward to the next symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'end) 'forward))
 
(defun smart-symbol-go-backward ()
  "Jumps backward to the previous symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'beginning) 'backward))
 
(defun smart-symbol-at-pt (&optional dir)
  "Returns the symbol at point and moves point to DIR (either `beginning' or `end') of the symbol.
 
If `smart-use-extended-syntax' is t then that symbol is returned
instead."
  (with-syntax-table (make-syntax-table)
    (if smart-use-extended-syntax
        (modify-syntax-entry ?. "w"))
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
    ;; grab the word and return it
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (if word
          (progn
            (cond
             ((eq dir 'beginning) (goto-char (car bounds)))
             ((eq dir 'end) (goto-char (cdr bounds)))
             (t (error "Invalid direction")))
            word)
        (error "No symbol found")))))

;;; simulate vi's "%" key
;;; copy from yinwang's emacs extensions.
(defun vi-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))


;;; Find pattern in files in the project
(defun project-rgrep ()
  (interactive)
  (let ((project-root (or ffip-project-root
			  (ffip-project-root)))
	(pat (symbol-at-point)))
    (if (or (null project-root)
	    (not (boundp 'ffip-file-exts))
	    (null pat))
	(call-interactively 'rgrep)
      (rgrep (symbol-name pat)
	     (mapconcat (lambda (x) (concat "*." x)) ffip-file-exts " ")
	     project-root))))


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

;; swap the key bindins for "C-x C-c" and "C-x C-q"
(global-set-key (kbd "C-x C-c") 'toggle-read-only)
(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)

(define-key emacs-lisp-mode-map (kbd "C-]") 'elisp-goto-definition-at-point)
(global-set-key "\C-ci" 'ido-goto-symbol) ; or any key you see fit
; the original "\M-x" binding is execute-extended-command
(global-set-key "\M-x" 'ido-execute-command)
(global-set-key "\M-X" 'execute-extended-command) ; binding it in case emgergent use
(global-set-key "\C-c*" 'occur-at-point)
(global-set-key "\C-cm" 'set-mark-command)
(if (eq system-type 'gnu/linux)
    (global-set-key (kbd "C-x C-S-f") 'find-file-in-project))
(global-set-key (kbd "C-x C-r") 'ido-choose-from-recentf)

;; smart jumping bindings
(global-set-key (kbd "M-n") 'smart-symbol-go-forward)
(global-set-key (kbd "M-p") 'smart-symbol-go-backward)

;; Lisp Interactive Mode bindings
(define-key lisp-interaction-mode-map (kbd "C-j") 'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-print-last-sexp)

;; simulate vi's "%" key
(global-set-key "%" 'vi-match-paren)

;; C-M-\ was bound to indent-region
(global-set-key (kbd "C-M-\\") 'comment-region)
;; C-M-/ was bound to dabbrev-completion
(global-set-key (kbd "C-M-/") 'uncomment-region)

(global-set-key '[f4] 'project-rgrep)
(global-set-key '[f5] 'compile)
(global-set-key '[f6] 'speedbar)
(global-set-key '[f10] 'gud-next)
(global-set-key '[f11] 'gud-step)
(global-set-key '[S-f11] 'gud-finish)

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

;; (global-set-key (kbd "M-p") 'previous-buffer)
;; (global-set-key (kbd "M-n") 'next-buffer)

;; Semantic
; "C-]" is originally bound to abort-recursive-edit
(define-key c-mode-base-map (kbd "C-]") 'semantic-ia-fast-jump)
; "C-t" is originally bound to transpose-chars
(define-key c-mode-base-map (kbd "C-t") 'semantic-mrub-switch-tags)
(define-key c-mode-base-map (kbd "C-=") 'semantic-analyze-proto-impl-toggle)
(global-set-key (kbd "C-M-m") 'eassist-list-methods)
(global-set-key (kbd "M-o") 'eassist-switch-h-cpp)

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

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "deep sky blue" :weight bold))))
 '(ido-first-match ((t (:background "black" :foreground "yellow" :weight bold))))
 '(ido-only-match ((((class color)) (:background "black" :foreground "yellow" :weight bold)))))
