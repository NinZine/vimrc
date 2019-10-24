;; Minimal UI
(scroll-bar-mode -1)
;(tool-bar-mode   -1)
(tooltip-mode    -1)
;(menu-bar-mode   -1)

(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-collection-setup-minibuffer t)
 '(geiser-default-implementation (quote chicken))
 '(global-linum-mode t)
 '(helm-mode t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (restclient anaconda-mode virtualenvwrapper ein evil-magit magit tide web-mode helm-projectile doom-themes use-package lispyville linum-relative ## rainbow-delimiters exec-path-from-shell cider evil-collection geiser racket-mode which-key helm evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Vim mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
	(setq evil-shift-width 2)
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
	 ("TAB" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 ("C-x C-f" . helm-find-files))
  :config (progn
	    (setq helm-mode-fuzzy-match t)
	    (setq helm-completion-in-region-fuzzy-match t)
	    (setq helm-autoresize-mode 1)
	    (setq helm-autoresize-max-height 0)
	    (setq helm-autoresize-min-height 20)))

(use-package helm-projectile
  :ensure t
  :bind (("C-c p" . helm-projectile-find-file)))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))


;; Racket
(use-package geiser
  :ensure t
  :config (setq geiser-active-implementations '(chicken))
  :init)

;; Evil collection
(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :ensure t
  :config (evil-collection-init))

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

;; Relative numbers
(use-package linum-relative
  :ensure t
  :init (global-linum-mode t))
  
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
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode))
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
	web-mode-enable-current-element-highlight t)

  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode)))))

;; Typescript
(use-package tide
  :ensure t
  :config
  (setq typescript-indent-level 2)
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-formater-before-save)))

;; Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;; Git plugin
(use-package magit
	:ensure t)

;; Vim bindings for magit
(use-package evil-magit
	:ensure t
	:after (magit evil-collection))

;; Python
(use-package ein
  :ensure t)

(use-package virtualenvwrapper
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode)))

;;'(python-shell-interpreter "jupyter")
;;'(python-shell-interpreter-args "console --simple-prompt")
;;'(python-shell-prompt-detect-failure-warning nil)

;; REST
(use-package restclient
  :ensure t)

;;; .emacs ends here
