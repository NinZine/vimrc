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
 ;; Fullscreen
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (doom-themes use-package lispyville linum-relative ## rainbow-delimiters exec-path-from-shell cider evil-collection geiser racket-mode which-key helm evil))))
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
  :config
  (evil-mode 1))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))


;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50))

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
  :init (linum-relative-mode))
  
;; Balancing parathensis nicely with evil-mode
(use-package lispyville
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
  (add-hook 'lisp-mode-hook #'lispyville-mode))
