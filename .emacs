
;;; Code:
(setq package-enable-at-startup nil)

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

(require 'gnutls)
(when (eq system-type 'darwin)
  (add-to-list 'gnutls-trustfiles "/opt/local/etc/openssl/cert.pem"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'aggressive)
 '(avy-style 'at-full)
 '(company-backends
   '(company-bbdb company-semantic company-cmake
		  (company-capf :with company-yasnippet) company-clang
		  company-files
		  (company-dabbrev-code company-gtags company-etags
					company-keywords)
		  company-oddmuse company-dabbrev company-yasnippet))
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
 '(evil-want-C-w-delete nil)
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(gdb-use-separate-io-buffer t)
 '(global-display-line-numbers-mode t)
 '(global-eldoc-mode t)
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
 '(org-publish-use-timestamps-flag nil)
 '(realgud-safe-mode nil)
 '(show-paren-mode t)
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history/")))
 '(warning-suppress-types '((use-package))))
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
 '(aw-leading-char-face
   ((t (:inherit avy-lead-face :height 3.0))))
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

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


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


;; Writing and organizing
(use-package org
  ;; :quelpa ((org :url "https://git.savannah.gnu.org/git/emacs/org-mode.git"
  ;; 		:fetcher git
  ;; 		:tag "release_9.6"
  ;; 		:files ("lisp/*.el" "doc/dir")))
  ;; :ensure t
  :hook ((org-mode . (lambda () (setq fill-column 80)))
	 (org-mode . auto-fill-mode)
	 (org-mode . olivetti-mode))
  :config (progn (evil-collection-define-key 'normal 'org-mode-map
		   (kbd "C-c t") 'org-todo)
		 (global-set-key (kbd "C-c o c") 'org-capture)))


;; Clipboard
(use-package exec-path-from-shell
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
  :if (and (not (eq system-type 'cygwin)) (not (eq system-type 'gnu/linux)) (eq (window-system) nil))
  :hook (after-init . global-clipetty-mode))

(use-package xclip
  :if (and (eq system-type 'gnu/linux) (eq (window-system) nil))
  :hook (after-init . xclip-mode))

(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil)
  :config (progn (setq dired-listing-switches "-alh")
		 (diredp-toggle-find-file-reuse-dir 1)))

(use-package emacs-async
  :config (setq dired-async-mode t))

;; Vim mode
(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package avy)

(use-package evil
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

(use-package ace-window
  :after evil
  :init (progn
	  (setq aw-leading-char-style 'path)
	  (setq aw-keys '(?h ?t ?s ?d ?i ?a ?o ?e ?u)))
  :config (define-key evil-window-map "a" 'ace-window))

;; Theme
(use-package doom-themes
  :config (progn
	    (setq-default line-spacing 0.25)
	    (setq doom-themes-enable-bold nil
		  doom-themes-enable-italic nil)
	    (load-theme 'doom-monokai-pro t)

	    ;; Transparent background
	    (set-face-background 'default "undefined")))

(use-package w3m
  :config
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))


;; Helm
(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 :map helm-map
	 ("TAB" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action))
  :config (progn
	    (setq helm-completion-style 'emacs)
	    (setq helm-minibuffer-history-key "M-p")
	    (setq helm-move-to-line-cycle-in-source nil)
	    (setq helm-mode-fuzzy-match t)
	    (setq helm-completion-in-region-fuzzy-match t)
	    (setq helm-autoresize-mode 1)
	    (setq helm-autoresize-max-height 0)
	    (setq helm-autoresize-min-height 20)
	    (if (executable-find "ugrep")
		(progn
		  (setq
		   grep-program "ugrep"
		   helm-grep-default-command "ugrep --color=always -a -d recurse %e -n%cH -e %p %f")))

	    (if (executable-find "rg")
		(setq helm-grep-ag-command
		      "rg --color=always --smart-case --search-zip --no-heading --line-number %s -- %s %s"))
	    (helm-mode 1)))

(use-package projectile
  :after helm
  :init
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  :config
  (projectile-mode))

(use-package helm-projectile
  ;; For Windows, install MSYS and put in the path so 'find' is available
  :after projectile
  :bind (("C-c p" . helm-projectile-find-file)))

(use-package helm-org-rifle
  :after helm
  :bind (("C-c o r" . helm-org-rifle)
	 ("C-c o d" . helm-org-rifle-org-directory)))

;; C/C++/ObjC
(use-package ede
  :hook ((c-mode-common . ede-minor-mode)))
(use-package rmsbolt)

(use-package lsp-mode :commands (lsp lsp-deferred)
  :after evil-collection
  :init (progn
	  ;; (setenv "LSP_USE_PLISTS" "true")
	  (setq gc-cons-threshold 100000000)
	  (setq read-process-output-max (* 1024 1024)) ;; 1mb
	  )
  ;; Code completion, documentation etc.
  ;; for python: pip install 'python-lsp-server[all]'
  :hook (((c-mode
	   c++-mode
	   objc-mode
	   cuda-mode
	   python-mode
	   tsx-ts-mode
	   typescript-ts-mode
	   typescript-mode
	   js-ts-mode
	   ) . lsp-deferred)
	 (c-mode-common . hs-minor-mode)
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . company-mode))
  :bind-keymap ("C-c l" . lsp-command-map)
  :config (progn
	    ;; (setq lsp-use-plists 't)
	    (setq lsp-pylsp-configuration-sources ["flake8" "pylsp-mypy"]
		  lsp-pylsp-plugins-mypy-enabled 't
		  lsp-completion-provider :none) ;; no provider because we want capf + yasnippet
	    (evil-collection-define-key 'normal 'lsp-mode-map
	      "K" 'lsp-describe-thing-at-point
	      "gr" 'lsp-find-references)
	    (evil-collection-define-key 'normal 'python-mode-map
	      "K" 'lsp-describe-thing-at-point
	      "gr" 'lsp-find-references
	      "gd" 'xref-find-definitions)
	    (evil-collection-define-key 'normal 'typescript-mode-map
	      "K" 'lsp-describe-thing-at-point
	      "gr" 'lsp-find-references
	      "gd" 'xref-find-definitions)
	    (evil-collection-define-key 'normal 'c-mode-base-map
	      "K" 'lsp-describe-thing-at-point
	      "gr" 'lsp-find-references
	      "zc" 'hs-hide-level)))

(use-package dap-mode :commands (dap-python)
  :straight (dap-mode :repo "emacs-lsp/dap-mode" :host github)
  :after lsp-mode
  :hook (dap-session-created . (lambda () (dap-hydra)))
  :config
  (require 'dap-python) ;; FIXME: This should be a hook to python-mode
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (setq dap-python-debugger 'debugpy))

(use-package helm-lsp)

;; When completion in some languages, parameters can be filled in by TAB
(use-package yasnippet
  :after lsp-mode
  :config (yas-global-mode))
(use-package yasnippet-snippets)

(use-package masm-mode)

(use-package nasm-mode)

(use-package kickasm-mode
  :straight (kickasm-mode :repo "mweidhagen/kickasm-mode" :host github)
  :mode (("\\.c64\\'" . kickasm-mode)))

;; XXX: Not recommended, it's better to use clangd
;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp)))
;;   :hook ((c-mode-common . hs-minor-mode))
;;   :config (setq ccls-executable "c:/msys64/usr/local/ccls.exe")
;;   (evil-define-key 'normal 'c-mode-base-map "zc" 'hs-hide-level))

;; Go
(use-package go-mode
  :if (executable-find "go"))

(use-package company-go
  :after go-mode)

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Common Lisp
(use-package slime
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
  :init (setq evil-collection-setup-minibuffer t)
  :config (progn (evil-collection-init)
		 (evil-collection-define-key 'normal 'dired-mode-map
	      (kbd "C-o") 'dired-open-file)))

;; CIDER for Clojure(Script)
;; (use-package cider
;;   :ensure t)

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlight parenthesis
(show-paren-mode 1)

;; Quiet the bell
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; Balancing parathensis nicely with evil-mode
(use-package lispyville
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
;; TODO: Replace with typescript-ts-mode, but grammar needs to be installed
(use-package typescript-mode)

;; Syntax checking
(use-package flycheck
  :init (global-flycheck-mode)
  :hook ((python-mode . (lambda () (setq flycheck-checker 'python-mypy)))
	 (typescript-mode . (lambda () (setq flycheck-check-syntax-automatically '(save mode-enabled))))))


;; Git plugin
(use-package magit)

(use-package magit-todos
  :hook ((magit-mode . magit-todos-mode))
  :config (setq global-hl-todo-mode t))

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
  :straight (pydoc :repo "statmobile/pydoc" :host github))

(use-package ein)

;; pyvenv-create to create a new env, pyvenv-workon and venv-work to use it
(use-package pyvenv
  :init (setenv "WORKON_HOME" "~/.pyenv/versions"))

;; For Windows, pip install pyreadline.
(setq python-shell-interpreter "python")
;;'(python-shell-interpreter-args "console --simple-prompt")
;;'(python-shell-prompt-detect-failure-warning nil)

(use-package blacken
  :straight (blacken :repo "pythonic-emacs/blacken" :host github)
  :hook ((python-mode . blacken-mode)))

(use-package company
  :straight (company-mode :repo "company-mode/company-mode" :host github)
  :bind (:map company-active-map
	 ("C-d" . company-show-doc-buffer))
  :config (progn (setq company-selection-default nil)
		 (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
	    
  :hook ((c-mode-common . company-mode)
	 (c-mode-common . (lambda () (setq-local company-backends '(company-c-headers company-yasnippet))))))

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode . lsp))

;; REST
(use-package restclient
  :ensure t)

;; Nim
(use-package nim-mode
  :straight (nim-mode :repo "nim-lang/nim-mode" :host github)
  :if (executable-find "nim")
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
  :straight (inim :repo "SerialDev/inim-mode" :host github)
  :after nim-mode)


(use-package org-journal
  :after org
  :config (global-set-key (kbd "C-c o j") 'org-journal-new-entry))

(use-package markdown-mode
  :hook ((markdown-mode . auto-fill-mode)
	 (markdown-mode . olivetti-mode)))

(use-package olivetti
  :straight (olivetti :repo "rnkn/olivetti" :host github)
  :hook ((olivetti-mode . (lambda () (setq olivetti-body-width 90)))))

(use-package llm
  :straight (llm :repo "ahyatt/llm" :host github)
  :if (executable-find "ollama"))

(use-package ellama
  :straight (ellama :repo "s-kostyaev/ellama" :host github)
  :after llm
  :bind ("C-c e" . ellama)
  :if (executable-find "ollama")
  :init
  (require 'llm-gemini)
  ;; (setopt ellama-keymap-prefix "C-c e")
  (setopt ellama-providers
	  '(("llama3" . (make-llm-ollama :chat-model "llama3" :embedding-model "llama3"))
	    ("gemini" . (make-llm-gemini :chat-model "gemini-2.5-pro-preview-05-06" :key (plist-get (car (auth-source-search :host "llm.gemini")) :secret))))))


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
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; if above code pollutes grep etc.
;; (ignore-errors
;;   (require 'ansi-color)
;;   (defun my-colorize-compilation-buffer ()
;;     (when (eq major-mode 'compilation-mode)
;;       (ansi-color-apply-on-region compilation-filter-start (point-max))))
;;   (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; Don't whine about a process running in the buffer, just kill it
(setq kill-buffer-query-functions nil)

;;; .emacs ends here
