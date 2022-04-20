
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
 '(company-box-enable-icon nil)
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
 '(kickasm-command-start-indent 2)
 '(kickasm-mnemonic-indent 2)
 '(kickasm-preprocessor-indent 0)
 '(kickasm-scoping-indent 2)
 '(kickasm-scoping-label-indent 0)
 '(org-agenda-files (list org-directory))
 '(org-capture-templates
   (quote
    (("l" "Log entry" plain
      (file+olp+datetree "z:/Org/log.org")))))
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-journal-dir "z:/Journal/")
 '(org-journal-file-format "%Y/%Y-%m-%d.org")
 '(package-selected-packages
   '(yasnippet helm-lsp company-c-headers masm-mode undo-tree olivetti org-journal nasm-mode org company-go go-mode pydoc virtualenvwrapper company-quickhelp pos-tip blacken company-box frame-local company-anaconda anaconda-mode helm-org-rifle nov company-mode nim-mode flycheck-nimsuggest company inim quelpa-use-package pyvenv helm-projectile tide web-mode use-package ## rainbow-delimiters exec-path-from-shell))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (when (eq system-type 'windows-nt)
   '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 98 :width normal))))))

;; Save backups and auto-save files to temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Do not open new dired buffers when navigating
(put 'dired-find-alternate-file 'disabled nil)

;; Column number mode, show column numbers
(setq column-number-mode t)

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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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
  :init (progn
	  (setq evil-undo-system 'undo-tree)
	  (setq evil-want-keybinding nil)
	  ;; Fix for TAB when not in GUI
	  (setq evil-want-C-i-jump nil)
	  (setq evil-shift-width 2))
  :config
  (evil-mode 1))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (setq-default line-spacing 0.25)
  (load-theme 'doom-vibrant t))


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
  :after helm)

;; C/C++/ObjC
(use-package ede
  :hook ((c-mode-common . ede-minor-mode)))

(use-package lsp-mode :commands lsp
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp)
  :hook ((c-mode-common . hs-minor-mode))
  :config (evil-define-key 'normal 'c-mode-base-map "zc" 'hs-hide-level))

(use-package helm-lsp
  :ensure t)

(use-package yasnippet
  :after lsp-mode
  :ensure t
  :hook ((c-mode-common . yas-minor-mode)))

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
		 ;; GUD key bindings
		 (evil-collection-define-key 'normal 'gud-minor-mode-map
		   (kbd "<f4>") 'gud-print
		   (kbd "<f5>") 'gud-cont
		   (kbd "S-<f5>") 'gud-kill
		   (kbd "<S-f6>") 'gud-tbreak
		   (kbd "<f6>") 'gud-break
		   (kbd "<f7>") 'gud-step
		   (kbd "<f8>") 'gud-next
		   (kbd "<f9>") 'gud-until
		   (kbd "S-<f8>") 'gud-finish)))

;; CIDER for Clojure(Script)
(use-package cider
  :ensure t)

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
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  :config
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
	web-mode-enable-current-element-highlight t))

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
	:ensure t)

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
(use-package pydoc
  :quelpa (pydoc :repo "whitypig/pydoc" :fetcher github))

(use-package ein
  :ensure t)

(use-package pyvenv
  :ensure t)

(use-package anaconda-mode
  :quelpa (anaconda-mode :repo "pythonic-emacs/anaconda-mode" :fetcher github)
  ;; For Windows, pip install pyreadline.
  ;; pyvenv-create to create a new env, pyvenv-workon and venv-work to use it
  :config (progn
	    (setq python-shell-interpreter "ipython")
	    (setq python-shell-interpreter-args "--simple-prompt -i "))
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode)))

;;'(python-shell-interpreter "jupyter")
;;'(python-shell-interpreter-args "console --simple-prompt")
;;'(python-shell-prompt-detect-failure-warning nil)

(use-package isortify
  :quelpa (isortify :repo "pythonic-emacs/isortify" :fetcher github)
  :hook ((python-mode . isortify-mode)))

(use-package blacken
  :quelpa (blacken :repo "pythonic-emacs/blacken" :fetcher github)
  :hook ((python-mode . blacken-mode)))

(use-package company
  :quelpa (company-mode :repo "company-mode/company-mode" :fetcher github)
  :ensure t
  :hook ((c-mode-common . company-mode)
	 (c-mode-common . (lambda () (setq-local company-backends '(company-gtags company-c-headers))))))

(use-package company-anaconda
  :quelpa (company-anaconda :repo "pythonic-emacs/company-anaconda" :fetcher github)
  :after company
  :hook ((python-mode . (lambda () (add-to-list 'company-backends 'company-anaconda)))
	 (python-mode . company-mode)))

(use-package company-box
  :quelpa (company-box :repo "sebastiencs/company-box" :fetcher github)
  :after company
  :hook (company-mode . company-box-mode))

;; REST
(use-package restclient
  :ensure t)

;; Nim
(use-package nim-mode
  :quelpa (nim-mode :repo "nim-lang/nim-mode" :fetcher github)
  :hook ((nim-mode . nimsuggest-mode)
	 (nim-mode . flymake-mode)
	 (nim-mode . company-mode)
	 ;; Disable flycheck-mode because infinite loop
	 (nim-mode . (lambda () (flycheck-mode -1))))
  :config (progn
	    (setq nimsuggest-accept-process-delay 50)
	    (setq nimsuggest-accept-process-timeout-count 100)
	    ;(add-hook 'nim-mode-hook #'(lambda () (flycheck-mode -1)))
	    ;(setq-default flycheck-disabled-checkers '(nim nim-nimsuggest))
	    (evil-collection-define-key 'normal 'nim-mode-map
	    "gd" 'xref-find-definitions)))

(use-package inim
  :quelpa (inim :repo "SerialDev/inim-mode" :fetcher github)
  :after nim-mode)

;; Writing and organizing
(use-package org
  :hook ((org-mode . (lambda () (setq fill-column 80)))
	 (org-mode . auto-fill-mode)
	 (org-mode . olivetti-mode)))

(use-package org-journal
  :ensure t
  :after org)

(use-package markdown
  :hook ((markdown-mode . auto-fill-mode)
	 (markdown-mode . olivetti-mode)))

(use-package olivetti
  :quelpa (olivetti :repo "rnkn/olivetti" :tag "1.11.2" :fetcher github)
  :hook ((olivetti-mode . (lambda () (setq olivetti-body-width 90)))))

;;; .emacs ends here
