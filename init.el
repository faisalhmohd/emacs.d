

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
(setq package-list `(neotree web-mode color-theme-sanityinc-tomorrow projectile ido-ubiquitous smex markdown-mode ac-js2 auto-complete all-the-icons rainbow-delimiters rainbow-mode))

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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#424242"))
 '(custom-enabled-themes (quote (firecode)))
 '(custom-safe-themes
   (quote
    ("18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(fci-rule-color "#424242")
 '(package-selected-packages
   (quote
    (firecode-theme rainbow-mode rainbo-mode rainbow-delimiters all-the-icons auto-complete autocomplete autocompletee ac-js2 markdown-mode smex ido-ubiquitous projectile color-theme-sanityinc-tomorrow neotree web-mode use-package)))
 '(projectile-mode t nil (projectile))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#2f2f2f" :foreground "#fdfdfd")))))

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
(add-to-list 'load-path "~/.emacs.d/nodejs-repl")
(load "node-js-repl.el")
(require 'nodejs-repl)

;; Show Line Numbers
(global-linum-mode t)

;; Hiding startup splash screen
(setq inhibit-startup-screen t)

;; Enabling Yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet")
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

;; Setting Yas Expand
(global-set-key [C-tab] 'yas-expand)

;; Enable Rainbow Delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Adding all-the-icons
(require 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Always display neotree and hidden files
(setq-default neo-show-hidden-files t)
(neotree-toggle)

;; Enable Rainbow mode globally
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))

(my-global-rainbow-mode 1)

