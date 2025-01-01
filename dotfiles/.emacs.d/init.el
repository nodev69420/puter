(setq custom-file "~/.emacs.d/custom.el")
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)
(column-number-mode 1)
(setq-default cursor-in-non-selected-windows nil)

(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; Transparency
;; (set-frame-parameter nil 'alpha-background 95)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)

(global-display-line-numbers-mode 1)
(set-default 'truncate-lines t)

(global-prettify-symbols-mode 1)

(setq blink-cursor-interval 0.15)
(setq blink-cursor-blinks -1)
(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)
(global-hl-line-mode)

(setq display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq browse-url-browser-function #'browse-url-firefox)

(setq debug-on-error t)
(setq edebug-all-forms t)
(setq backtrace-depth 50)
(setq message-log-max 16384)
(setq log-max (expt 2 22))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq make-backup-files t)
(setq version-control nil)
(setq backup-by-copying t)
(setq vc-make-backup-files t)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(electric-pair-mode 1)
(setq compile-command "") 

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold (* 100 1024 1024))

(defun adam/qoutize-string (str)
  "Surround a string in \"\" qoutes."
  (concat "\"" str "\""))

(defvar adam/font "JetBrainsMono Nerd Font Mono")

(defun adam/set-font (name)
  "Set the font globally."
  (setq adam/font name))

(defun adam/set-font-size (font-size)
  "Set the font size globally."
  (let ((font-height (* font-size 10)))
    (set-face-attribute
     'default nil
     :font adam/font :height font-height)
    (set-face-attribute
     'variable-pitch nil
     :font adam/font :height font-height)
    (set-face-attribute
     'fixed-pitch nil
     :font adam/font :height font-height))
  (let ((font-frame (concat adam/font "-" (number-to-string font-size))))
    (add-to-list
     'default-frame-alist
     `(font . ,font-frame))))

(adam/set-font-size 13)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun adam/enable-fast-keys ()
  "Enable fast typing."
  (interactive)
  (shell-command-to-string "xset r rate 200 80"))

(defun adam/goto-init-file ()
  "Open init file."
  (interactive)
  (find-file user-init-file))

(defun adam/reload-init-file ()
  "Reload emacs config."
  (interactive)
  (load-file user-init-file))

(defun adam/fuzzy-find ()
  "Fuzzy find based on the contents of the current buffer."
  (interactive)
  (cond ((eq major-mode 'dired-mode)
         (call-interactively #'counsel-find-file))
        ((eq major-mode 'ibuffer-mode)
         (call-interactively #'counsel-ibuffer))
        ((eq major-mode 'eshell-mode)
         (call-interactively #'counsel-find-file))
        (t (call-interactively #'counsel-imenu))))

(defun adam/display-startup-time ()
  "Display emacs starting time."
  (message "Emacs loaded in: %s, gc collects: %d."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'adam/display-startup-time)

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)
(use-package counsel
  :init (counsel-mode 1)
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode t)
  (use-package swiper)
  (use-package flx)
  (use-package ivy
    :bind (("C-s" . swiper)
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :custom
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq enable-recursive-minibuffers t)
    :config
    (use-package org-ivy-search)
    (ivy-mode 1)
    (setq ivy-height 20)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))


(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package all-the-icons)
(use-package all-the-icons-completion)
(use-package all-the-icons-dired)
(use-package all-the-icons-ibuffer)
(use-package all-the-icons-ivy)
(use-package all-the-icons-nerd-fonts)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic nil)
  ;; (load-theme 'doom-homage-black t))
  (setq doom-winter-is-coming-no-italics t)
  (setq doom-winter-is-coming-brighter-comments t)
  (load-theme 'doom-winter-is-coming-dark-blue t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28)
  (setq doom-modeline-enable-word-count t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-minibuffer nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map
              (kbd "C-u")
              #'(lambda ()
                  (interactive)
                  (call-interactively #'recenter)
                  (evil-scroll-up 16)))
  (define-key evil-normal-state-map
              (kbd "C-d")
              #'(lambda ()
                  (interactive)
                  (call-interactively #'recenter)
                  (evil-scroll-down 16)))
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-lispy
  :after evil
  :config
  (add-hook 'evil-mode-hook #'evil-lispy-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer adam/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (adam/leader-keys
    "s" '(:ignore t :wk "Search")
    "ss" '(swiper :wk "Search Swiper")
    "SPC" '(counsel-M-x :wk "M-x")
    "f" '(:ignore t :wk "Find")
    "fc" '(adam/goto-init-file :wk "Find Config")
    "ff" '(adam/fuzzy-find :wk "Find Fuzzy-Contextual")
    "fa" '(lsp-ivy-workspace-symbol :wk "Find LSP Symbol")
    "f." '(counsel-find-file :wk "Find File")
    "gg" '(magit :wk "Magit")
    "b" '(:ignore t :wk "Buffer")
    "bb" '(counsel-ibuffer :wk "Buffer Switch")
    "bm" '(ibuffer :wk "Buffer Menu")
    "bx" '(kill-this-buffer :wk "Buffer Kill")
    "e" '(:ignore t :wk "Eval")
    "ee" '(eval-expression :wk "Eval Expression")
    "eb" '(eval-buffer :wk "Eval Buffer")
    "ex" '(eval-last-sexp :wk "Eval Last Sexpr")
    "h" '(:ignore t :wk "Help")
    "hf" '(counsel-describe-function :wk "Help Function")
    "hv" '(counsel-describe-variable :wk "Help Variable")
    "l" '(:ignore t :wk "LSP")
    "lr" '(lsp-rename :wk "LSP Rename")
    "lf" '(lsp-find-references :wk "LSP Find References")
    "ld" '(lsp-treemacs-errors-list :wk "LSP Diagnostics")
    "w" '(:ignore t :wk "Window")
    "w1" '(delete-other-windows :wk "Window Solo")
    "wn" '(evil-window-split :wk "Window Split Horizontal")
    "wv" '(evil-window-vsplit :wk "Window Split Vertical")
    "ww" '(evil-window-next :wk "Window Next")
    "wc" '(evil-window-delete :wk "Window Close")
    "wx" '(kill-buffer-and-window :wk "Window Kill and Close")
    "wh" '(evil-window-left :wk "Window Left")
    "wj" '(evil-window-down :wk "Window Down")
    "wk" '(evil-window-up :wk "Window Up")
    "wl" '(evil-window-right :wk "Window Right")
    "c" '(:ignore t :wk "Command")
    "ce" '(eshell :wk "Command Eshell")
    "cc" '((lambda ()
               (interactive)
               (call-interactively #'compile))
           :wk "Command Current")
    "cp" '((lambda ()
               (interactive)
               (when (projectile-project-p)
                   (call-interactively #'projectile-compile-project)))
               :wk "Command Project")
    "cn" '((lambda ()
               (interactive)
               (setq compile-command "")
               (call-interactively #'compile))
           :wk "Command New")
    "r" '(:ignore t :wk "Reload")
    "rc" '(adam/reload-init-file :wk "Reload Config")))

(use-package projectile
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work")
    (setq projectile-project-search-path '("~/work")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-max-display-columns nil
        which-key-min-display-lines 6))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package org
  :config
  (setq org-edit-src-content-indentation 0))

(use-package lsp-mode
  :init
  (setq lsp-log-io nil)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-clients-clangd-args '("--header-insertion=never")))

(use-package lsp-ivy)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("C-l" . company-complete-selection)
        ("<return>" . nil)
        ("RET" . nil))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; (use-package zig-mode
;;   :config
;;   (add-hook 'zig-mode-hook 'lsp-mode)
;;   (add-hook 'zig-mode-hook
;;             #'(lambda ()(interactive)(zig-format-on-save-mode -1))))

(use-package glsl-mode)
(use-package cmake-mode)
(use-package i3wm-config-mode)
(use-package lua-mode)

(use-package sly
  :config
  (setq inferior-lisp-program "/bin/sbcl"))

(use-package fennel-mode)

(use-package gdscript-mode
  :config
  (add-hook 'gdscript-mode-hook 'lsp-mode)
  (setq gdscript-godot-executable "/bin/godot/godot"))

(use-package hydra)

(use-package evil-nerd-commenter
  :bind ("C-;" . evilnc-comment-or-uncomment-lines))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  ((dired-listing-switches "-lah --group-directories-first"))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "R" 'dired-do-rename
    "C" 'dired-do-copy
    "D" 'dired-do-delete)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions
        '(("gif" . "feh")
          ("jpg" . "feh")
          ("png" . "feh")
          ("mkv" . "mpv")
          ("mp4" . "mpv")
          ("webm" . "mpv")
          ("xcf" . "gimp")
          ("pdf" . "firefox")
          ("kra" . "krita")
          ("blend" . "blender"))))

(use-package dired-hide-dotfiles
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package eshell
  :config
  (mapc (lambda (alias) (defalias (car alias) (cdr alias)))
        '((ll . (lambda () (eshell/ls '-lah)))
          (dir . dired))))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package magit)
(use-package forge)

(use-package cc-mode
  :config
  (add-hook 'c-mode-hook 'lsp-mode)
  (add-hook 'c++-mode-hook 'lsp-mode))

;; Email


(define-minor-mode adam-mode
  "Adam global mode for Adam based sheringans!"
  1
  :global t
  :group 'adam
  :lighter " adam-mode")

(load-file custom-file)
