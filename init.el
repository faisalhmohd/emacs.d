(require 'package)

;; Setting package repositories
(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(package-initialize)

;; Packages used
;; Add your package here then execute eval-buffer
(setq package-list `(neotree web-mode color-theme-sanityinc-tomorrow projectile ido-ubiquitous smex markdown-mode ac-js2 auto-complete))

;; Fetch available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages if not installed
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Removing Menu Bar, Toolbar and Scroll Bar

(menu-bar-mode -1)
(toggle-scroll-bar -1) 
(tool-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(package-selected-packages
   (quote
    (auto-complete autocomplete autocompletee ac-js2 markdown-mode smex ido-ubiquitous projectile color-theme-sanityinc-tomorrow neotree web-mode use-package)))
 '(projectile-mode t nil (projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load theme
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-bright)

;; Turning off Alarm Bell
(setq ring-bell-function 'ignore)

;; Initialize Projectile Mode
(projectile-global-mode)

;; Ido Mode

;; Enable
(ido-mode t)

;; Enable partial matches
(setq ido-enable-flex-matching t)

;; Disable filename at point
(setq ido-use-filename-at-point nil)

;; Only match buffers in the current working directory
(setq ido-auto-merge-work-directories-length -1)

;; Display recently opened buffers (Even if closed!)
(setq ido-use-virtual-buffers t)

;; Set ido everywhere
(ido-ubiquitous-mode 1)

;; Modify existing buffer behaviour
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Enable Markdown Mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Enabling NodeJS REPL
(add-to-list 'load-path "~/.emacs.d/")
(load "node-js-repl.el")
(require 'nodejs-repl)

;; Show Line Numbers
(global-linum-mode t)

;; Hiding startup splash screen
(setq inhibit-startup-screen t)

;; Enabling Yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Enabling Auto Complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Setting ac-js2 and tern
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(if (eq system-type 'windows-nt)
    (setq tern-command '("node" "\\usr\\local\\bin\\tern")))
(eval-after-load 'tern
    '(progn
    (require 'tern-auto-complete)
    (tern-ac-setup)))
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'tern-mode)

;; Syntax Highlighting for Browser languages.
;; Formatting for JS and JSX 
(require 'web-mode)
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(put 'upcase-region 'disabled nil)

;; Prompt y and n instead of yes and no
(defalias 'yes-or-no-p 'y-or-n-p)
