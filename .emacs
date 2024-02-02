
;;; Code:
;; Minimal UI
(when (not (eq (window-system) nil))
  (progn
    ;(menu-bar-mode   -1)
    (scroll-bar-mode -1)
    (tool-bar-mode   -1)
    (tooltip-mode    -1)))

;; Linux/Mac
(when (not (eq system-type 'windows-nt))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

;; MSYS2, mingw32
(when (eq system-type 'windows-nt)
  (setq exec-path (append exec-path '("c:/msys64/mingw64/bin" "c:/msys64/usr/bin")))
  (setenv "PATH" (concat "c:\\msys64\\mingw64\\bin" ";" "c:\\msys64\\usr\\bin" ";" (getenv "PATH"))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
(require 'gnutls)
(when (eq system-type 'darwin)
  (add-to-list 'gnutls-trustfiles "/opt/local/etc/openssl/cert.pem"))

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'aggressive)
 '(avy-style 'at-full)
 '(company-box-enable-icon nil)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(desktop-path '("." "~/.emacs.d/" "~"))
 '(desktop-restore-eager t)
 '(desktop-save 'if-exists)
 '(desktop-save-mode t)
 '(display-line-numbers 'visual)
 '(display-line-numbers-current-absolute t)
 '(display-line-numbers-type 'visual)
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(gdb-use-separate-io-buffer t)
 '(global-display-line-numbers-mode t)
 '(global-eldoc-mode t)
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(helm-mode t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(help-window-select t)
 '(kickasm-command "java cml.kickass.KickAssembler")
 '(kickasm-command-start-indent 2)
 '(kickasm-indent-labels-to-scoping-level t)
 '(kickasm-mnemonic-indent 2)
 '(kickasm-preprocessor-indent 0)
 '(kickasm-preprocessor-indentation-mode '##)
 '(kickasm-scoping-indent 2)
 '(kickasm-scoping-label-indent 0)
 '(magit-todos-exclude-globs '("*.map" ".git/"))
 '(org-capture-templates
   (quote
    (("l" "Log entry" plain
      (file+olp+datetree "z:/Org/log.org")))))
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-journal-dir "z:/Journal/")
 '(org-agenda-files (list org-directory))
 '(org-journal-file-format "%Y/%Y-%m-%d.org")
 '(package-selected-packages
   '(yasnippet helm-lsp company-c-headers masm-mode undo-tree olivetti org-journal nasm-mode org company-go go-mode pydoc virtualenvwrapper company-quickhelp pos-tip blacken company-box frame-local company-anaconda anaconda-mode helm-org-rifle nov company-mode nim-mode flycheck-nimsuggest company inim quelpa-use-package pyvenv helm-projectile tide web-mode use-package ## rainbow-delimiters exec-path-from-shell))
 '(show-paren-mode t))
 '(show-paren-mode t)
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (when (eq system-type 'windows-nt)
   '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 98 :width normal))))))
 '(avy-goto-char-timer-face ((t (:inherit highlight :background "brightyellow" :foreground "black"))))
 '(avy-lead-face ((t (:background "color-93" :foreground "brightyellow" :weight bold))))
 '(avy-lead-face-0 ((t (:inherit avy-lead-face :background "color-21" :foreground "brightyellow"))))
 '(avy-lead-face-1 ((t (:inherit avy-lead-face :background "color-21" :foreground "brightyellow"))))
 '(avy-lead-face-2 ((t (:inherit avy-lead-face :background "color-21" :foreground "brightyellow"))))
 '(header-line ((t (:inherit mode-line :background "nil"))))
 '(kickasm-mnemonic-face ((t (:foreground "DarkOrange3" :slant normal))))
 '(kickasm-mnemonic-slant-face ((t (:inherit normal))))
 '(kickasm-unintended-mnemonic-face ((t (:foreground "red3" :slant normal)))))

;; Save backups and auto-save files to temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Do not open new dired buffers when navigating
(put 'dired-find-alternate-file 'disabled nil)

;; Column number mode, show column numbers
(setq column-number-mode t)

;; Fix restoring desktop windows
;; https://emacs.stackexchange.com/questions/19190/desktop-save-mode-fails-to-save-window-layout
(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
 (lambda ()
   (frameset-restore
    desktop-saved-frameset
    :reuse-frames (eq desktop-restore-reuses-frames t)
    :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
    :force-display desktop-restore-in-current-display
    :force-onscreen desktop-restore-forces-onscreen)))

;; GDB tweaks
(advice-add 'gdb-setup-windows :after
            (lambda () (set-window-dedicated-p (selected-window) t)))

;; Quelpa
(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

;; Bootstrap `use-package`
(unless (package-installed-p 'quelpa-use-package)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))
(require 'quelpa-use-package)
(setq use-package-ensure-function 'quelpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Helper for compilation. Close the compilation window if
;; there was no error at all. (emacs wiki)
(defun compilation-exit-autoclose (status code msg)
  "Hide compilation output if successful (after 1 second)."
  ;; If M-x compile exits with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (let ((compilation-buffer (current-buffer)))
    (run-with-timer 1 nil
		    (lambda (buf)
		      (when (and 't (not (string-match "rmsbolt" (buffer-name buf))))
			(bury-buffer buf)
			(delete-window (get-buffer-window buf))))
		    compilation-buffer)))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

(defun compilation-split-window (buffer &optional args)
  "Splits window when compiling."
  ;; Comments are for using the compile window at the bottom instead of top
  ;;(let* ((w (selected-window)))
  (let* ((w (split-window)))
        ;;(select-window (split-window))
        (switch-to-buffer buffer)
        (get-buffer-window buffer 0)
	(select-window w)))

(setq display-buffer-alist
      `(("*compilation*" (display-buffer-pop-up-window display-buffer-reuse-window compilation-split-window))
	("*kickasm*" (display-buffer-pop-up-window display-buffer-reuse-window compilation-split-window))
	("*nim-compile*" (display-buffer-pop-up-window display-buffer-reuse-window compilation-split-window))
	("*grep*" (display-buffer-pop-up-window switch-to-buffer))))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "open" nil 0 nil file)))


(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; Fix `cygstart` not found in MSYS when opening URLs
(when (eq system-type 'cygwin)
  (progn
    (setq
     browse-url-browser-function
     (lambda (url &optional _new-window)
       (interactive (browse-url-interactive-arg "URL: "))
       (shell-command (concat "start " (shell-quote-argument url)()))))
    ;; Copy/cut/paste to Windows from terminal in MSYS
    (setq
     interprogram-cut-function
     (lambda (text)
       (with-temp-buffer
	 (insert text)
	 ;; XXX: With clip.exe if you prefer
	 ;; (call-process-region (point-min) (point-max) "clip.exe" t t nil))))
	 (write-region (point-min) (point-max) "/dev/clipboard" nil 'silent))))
    (setq
     interprogram-paste-function
     (lambda ()
       ;; Remove ^M characters before pasting
       (let ((clip-output (replace-regexp-in-string "" "" (shell-command-to-string "cat /dev/clipboard"))))
	 (unless (string= (car kill-ring) clip-output)
	   clip-output))))))

(use-package clipetty
  :ensure t
  :if (and (not (eq system-type 'cygwin)) (not (eq system-type 'gnu/linux)) (eq (window-system) nil))
  :hook (after-init . global-clipetty-mode))

(use-package xclip
  :ensure t
  :if (and (eq system-type 'gnu/linux) (eq (window-system) nil))
  :hook (after-init . xclip-mode))

;; Vim mode
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package evil
  :ensure t
  :after undo-tree
  :hook (evil-local-mode . turn-on-undo-tree-mode)
  :init (setq
	 evil-mode-line-format nil
	 evil-undo-system 'undo-tree
	 evil-want-keybinding nil
	 ;; Fix for TAB when not in GUI
	 evil-want-C-i-jump nil
	 evil-shift-width 2)
  :config (progn
	    (define-key evil-normal-state-map (kbd "C-_") 'avy-goto-char)
	    (evil-mode 1))
  :hook (evil-local-mode . turn-on-undo-tree-mode))

;; Theme
(use-package doom-themes
  :ensure t
  :config (progn
	    (setq-default line-spacing 0.25)
	    (setq doom-themes-enable-bold nil
		  doom-themes-enable-italic nil)
	    (load-theme 'doom-monokai-pro t)

	    ;; Transparent background
	    (set-face-background 'default "undefined")))


;; Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 :map helm-map
	 ("TAB" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action))
  :config (progn
	    (setq helm-mode-fuzzy-match t)
	    (setq helm-completion-in-region-fuzzy-match t)
	    (setq helm-autoresize-mode 1)
	    (setq helm-autoresize-max-height 0)
	    (setq helm-autoresize-min-height 20)))

(use-package projectile
  :ensure t
  :after helm
  :init
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  :config
  (projectile-mode))

(use-package helm-projectile
  ;; For Windows, install MSYS and put in the path so 'find' is available
  :ensure t
  :after projectile
  :bind (("C-c p" . helm-projectile-find-file)))

(use-package helm-org-rifle
  :ensure t
  :after helm
  :bind (("C-c o r" . helm-org-rifle)
	 ("C-c o d" . helm-org-rifle-org-directory)))

;; C/C++/ObjC
(use-package ede
  :hook ((c-mode-common . ede-minor-mode)))
(use-package rmsbolt
  :quelpa)

(use-package lsp-mode :commands lsp
  :ensure t
  :after evil-collection
  ;; Code completion, documentation etc.
  ;; for python: pip install 'python-lsp-server[all]'
  :hook ((c-mode c++-mode objc-mode cuda-mode python-mode) . lsp)
  :hook ((c-mode-common . hs-minor-mode))
  :config (progn
	    (evil-collection-define-key 'normal 'lsp-mode-map
	      "K" 'lsp-describe-thing-at-point
	      "gr" 'lsp-find-references)
	    (evil-collection-define-key 'normal 'python-mode-map
	      "K" 'lsp-describe-thing-at-point
	      "gr" 'lsp-find-references
	      "gd" 'xref-find-definitions)
	    (evil-collection-define-key 'normal 'c-mode-base-map
	      "K" 'lsp-describe-thing-at-point
	      "gr" 'lsp-find-references
	      "zc" 'hs-hide-level)))

(use-package dap-mode :commands (dap-python)
  :quelpa (dap-mode :repo "emacs-lsp/dap-mode" :fetcher github)
  :after lsp-mode
  :ensure t
  :hook (dap-session-created . (lambda () (dap-hydra)))
  :config
  (require 'dap-python) ;; FIXME: This should be a hook to python-mode
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (setq dap-python-debugger 'debugpy))

(use-package helm-lsp
  :ensure t)

;; When completion in some languages, parameters can be filled in by TAB
(use-package yasnippet
  :after lsp-mode
  :ensure t
  :config (yas-global-mode))

(use-package masm-mode
  :ensure t)

(use-package nasm-mode
  :ensure t)

(use-package kickasm-mode
  :quelpa (kickasm-mode :repo "mweidhagen/kickasm-mode" :fetcher github)
  :mode (("\\.c64\\'" . kickasm-mode)))

;; XXX: Not recommended, it's better to use clangd
;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp)))
;;   :hook ((c-mode-common . hs-minor-mode))
;;   :config (setq ccls-executable "c:/msys64/usr/local/ccls.exe")
;;   (evil-define-key 'normal 'c-mode-base-map "zc" 'hs-hide-level))

;; Go
(use-package go-mode
  :ensure t)

(use-package company-go
  :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Common Lisp
(use-package slime
  :ensure t
  :init (progn
	  ;; (setq inferior-lisp-program "sbcl.exe")
	  (setq slime-default-lisp "sbcl")
	  (setq slime-lisp-implementations
		'((ecl ("c:/msys64/usr/local/ecl.exe"))
		  (sbcl ("c:/Program Files/Steel Bank Common Lisp/sbcl.exe")))))
  :config (setq slime-contribs '(slime-fancy slime-quicklisp slime-asdf)))

;; Evil collection
(use-package evil-collection
  :after evil
  :ensure t
  :init (setq evil-collection-setup-minibuffer t)
  :config (progn (evil-collection-init)
		 (evil-collection-define-key 'normal 'dired-mode-map
	      (kbd "C-o") 'dired-open-file)))

;; CIDER for Clojure(Script)
;; (use-package cider
;;   :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlight parenthesis
(show-paren-mode 1)

;; Quiet the bell
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; Balancing parathensis nicely with evil-mode
(use-package lispyville
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
  (add-hook 'lisp-mode-hook #'lispyville-mode)
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (with-eval-after-load 'lispyville
    (lispyville-set-key-theme
     '(operators
       c-w
       (escape insert)
       (additional-movement normal visual motion)
       slurp/barf-lispy))))


;; HTML development
(use-package web-mode
  :quelpa t
  :mode (("\\.html?\\'" . web-mode))
  :config (progn
	    (setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 2
		  css-indent-offset 2
		  web-mode-code-indent-offset 2
		  web-mode-block-padding 2
		  web-mode-comment-style 2

		  web-mode-enable-css-colorization t
		  web-mode-enable-auto-pairing t
		  web-mode-enable-comment-keywords t
		  web-mode-enable-auto-quoting nil
		  web-mode-enable-current-element-highlight t)
	    ;; C-t overshadowed by my tmux prefix
	    (evil-collection-define-key 'normal 'web-mode-map
	      (kbd "C-c t a") 'web-mode-tag-attributes-sort
	      (kbd "C-c t b") 'web-mode-tag-beginning
	      (kbd "C-c t e") 'web-mode-tag-end
	      (kbd "C-c t m") 'web-mode-tag-match
	      (kbd "C-c t n") 'web-mode-tag-next
	      (kbd "C-c t p") 'web-mode-tag-previous
	      (kbd "C-c t s") 'web-mode-tag-select)))

;; Typescript
(use-package tide
  :ensure t
  :mode (("\\.tsx?\\'" . typescript-mode)
	 ("\\.jsx?\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2)
  :after (typescript-mode flycheck company)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . company-mode)
	 (typescript-mode . eldoc-mode)
	 (typescript-mode . flycheck-mode)
	 (typescript-mode . web-mode)
         (before-save . tide-format-before-save)))

;; Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook ((python-mode . (lambda () (setq flycheck-checker 'python-mypy)))
	 (typescript-mode . (lambda () (setq flycheck-check-syntax-automatically '(save mode-enabled))))))


;; Git plugin
(use-package magit
  :quelpa magit)

(use-package magit-todos
  :ensure t
  :hook ((magit-mode . magit-todos-mode)))

;; Python
;; Fixes for not getting echo in run-python
(defun python-shell-append-to-output (string)
  (let ((buffer (current-buffer)))
    (set-buffer (process-buffer (python-shell-get-process)))
    (let ((oldpoint (point)))
      (goto-char (process-mark (python-shell-get-process)))
      (insert string)
      (set-marker (process-mark (python-shell-get-process)) (point))
      (goto-char oldpoint))
    (set-buffer buffer)))

(defadvice python-shell-send-string
    (around advice-python-shell-send-string activate)
  (interactive)
  (let* ((append-string1
         (if (string-match "import codecs, os;__pyfile = codecs.open.*$" string)
             (replace-match "" nil nil string)
           string))
        (append-string2
         (if (string-match "^# -\\*- coding: utf-8 -\\*-\n*$" append-string1)
             (replace-match "" nil nil append-string1)
           append-string1))
        (append-string
         (if (string-match "^\n*$" append-string2)
             (replace-match "" nil nil append-string2)
           append-string2)))  
    (python-shell-append-to-output
     (concat (string-trim-right append-string) "\n")))
  (if (called-interactively-p 'any)
      (call-interactively (ad-get-orig-definition 'python-shell-send-string))
    ad-do-it))

;; Folding
(add-hook 'python-mode-hook 'hs-minor-mode)

(use-package pydoc
  :quelpa (pydoc :repo "statmobile/pydoc" :fetcher github :upgrade t))

(use-package ein
  :ensure t)

;; pyvenv-create to create a new env, pyvenv-workon and venv-work to use it
(use-package pyvenv
  :ensure t)

;; For Windows, pip install pyreadline.
(setq python-shell-interpreter "python")
;;'(python-shell-interpreter-args "console --simple-prompt")
;;'(python-shell-prompt-detect-failure-warning nil)

(use-package blacken
  :quelpa (blacken :repo "pythonic-emacs/blacken" :fetcher github)
  :hook ((python-mode . blacken-mode)))

(use-package company
  :quelpa (company-mode :repo "company-mode/company-mode" :fetcher github)
  :bind (:map company-active-map
	 ("C-d" . company-show-doc-buffer))
  :config (progn (setq company-selection-default nil)
		 (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
	    
  :hook ((c-mode-common . company-mode)
	 (c-mode-common . (lambda () (setq-local company-backends '(company-c-headers company-yasnippet))))))

;; REST
(use-package restclient
  :ensure t)

;; Nim
(use-package nim-mode
  :quelpa (nim-mode :repo "nim-lang/nim-mode" :fetcher github)
  :hook ((nim-mode . lsp)
	 (nim-mode . flymake-mode)
	 (nim-mode . company-mode)
	 ;; Disable flycheck-mode because infinite loop
	 (nim-mode . (lambda () (flycheck-mode -1))))
  :config (progn
	    (setq nimsuggest-accept-process-delay 50)
	    (setq nimsuggest-accept-process-timeout-count 100)
	    (evil-collection-define-key 'normal 'nim-mode-map
	      "K" 'lsp-describe-thing-at-point
	      "gd" 'xref-find-definitions)))

(use-package inim
  :quelpa (inim :repo "SerialDev/inim-mode" :fetcher github)
  :after nim-mode)

;; Writing and organizing
(use-package org
  :hook ((org-mode . (lambda () (setq fill-column 80)))
	 (org-mode . auto-fill-mode)
	 (org-mode . olivetti-mode))
  :config (progn (evil-collection-define-key 'normal 'org-mode-map
		   (kbd "C-c t") 'org-todo)
		 (global-set-key (kbd "C-c o c") 'org-capture)))

(use-package org-journal
  :ensure t
  :after org
  :config (global-set-key (kbd "C-c o j") 'org-journal-new-entry))

(use-package markdown
  :hook ((markdown-mode . auto-fill-mode)
	 (markdown-mode . olivetti-mode)))

(use-package olivetti
  :quelpa (olivetti :repo "rnkn/olivetti" :tag "1.11.2" :fetcher github)
  :hook ((olivetti-mode . (lambda () (setq olivetti-body-width 90)))))


;; GUD key bindings
(add-hook 'gud-mode-hook (lambda () (progn
  (define-key gud-minor-mode-map (kbd "<f4>") #'gud-print)
  (define-key gud-minor-mode-map (kbd "<f5>") #'gud-cont)
  (define-key gud-minor-mode-map (kbd "S-<f5>") #'gud-kill)
  (define-key gud-minor-mode-map (kbd "<f6>") #'gud-break)
  (define-key gud-minor-mode-map (kbd "<f7>") #'gud-step)
  (define-key gud-minor-mode-map (kbd "<f8>") #'gud-next)
  (define-key gud-minor-mode-map (kbd "<f9>") #'gud-until)
  (define-key gud-minor-mode-map (kbd "S-<f8>") #'gud-finish))))

(define-key global-map (kbd "C-c /") 'comment-line)

(defvar default-mode-line-foreground (face-foreground 'mode-line))
(defvar default-mode-line-background (face-background 'mode-line))
(defun change-mode-line-color ()
  (let* ((default-color (cons default-mode-line-background default-mode-line-foreground))
	 (color (cond ((minibufferp) default-color)
		      ((evil-insert-state-p) '("color-53" . "#ffffff"))
		      (t default-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))
(add-hook 'post-command-hook 'change-mode-line-color)

;; Refresh dired directories automatically
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)

;;; .emacs ends here
