;;; init.el --- leet-init-file                    -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :config
  (setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading initial packages...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(use-package dash
  :config (dash-enable-font-lock))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (setq recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(abbreviate-file-name))
  (recentf-mode))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(progn ;     General
  (defalias 'yes-or-no-p 'y-or-n-p))

(progn ;     UI
  (set-face-attribute 'default nil :font "Source Code Pro-14")
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;     UTF-8
  (set-charset-priority 'unicode)
  (setq locale-coding-system   'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system        'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(progn ;     files & buffers
  (setq confirm-kill-emacs 'y-or-n-p
        ;; use version control
        version-control t
        ;; don't ask for confirmation when opening symlinked file
        vc-follow-symlinks t
        ;; History & backup settings (save nothing, that's what git is for
        auto-save-list-file-prefix nil
        auto-save-default nil
        create-lockfiles nil
        history-length 500
        make-backup-files nil)
  (global-auto-revert-mode t)
  ;; Delete trailing whitespace
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun leet-project-root ()
  "Get the path to the root of your project."
  (let ((projectile-require-project-root))
    (ignore-errors (projectile-project-root))))

(use-package company
  :init
  (setq company-idle-delay 0)
  :config
  (add-hook 'prog-mode-hook #'company-mode))

(defun leet-init-flycheck-eslint ()
  "Favor local eslint over global."
  (when (derived-mode-p 'js-mode)
      (when-let ((exec-path (list (concat (leet-project-root) "node_modules/.bin")))
                 (eslint (executable-find "eslint")))
        (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'leet-init-flycheck-eslint)
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package undo-tree
  :config
  ;; persistent undo history is known to cause undo history corruption, which
  ;; can be very destructive! So disable it!
  (setq undo-tree-auto-save-history nil))

;;;BEGIN: evil
(use-package evil
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode t))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode))

(use-package evil-magit)

(use-package evil-anzu)
;;;END: evil

;;;BEGIN: User Interface
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-one t))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
	powerline-height 30)
  (set-face-attribute 'spaceline-evil-emacs nil :background "#dfdfdf")
  (set-face-attribute 'spaceline-evil-insert nil :background "#99bb66")
  (set-face-attribute 'spaceline-evil-motion nil :background "#ae81ff")
  (set-face-attribute 'spaceline-evil-normal nil :background "#51afef")
  (set-face-attribute 'spaceline-evil-replace nil :background "#da8548")
  (set-face-attribute 'spaceline-evil-visual nil :background "#c678dd")
  (spaceline-toggle-minor-modes-off)
  (spaceline-emacs-theme))

(use-package nlinum
  :config
  (add-hook 'after-init-hook #'global-nlinum-mode))

(use-package winum
  :config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode))

(use-package anzu
  :config
  (setq anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil :foreground "#51afef")
  (set-face-attribute 'anzu-mode-line-no-match nil :foreground "#bf5150")
  (global-anzu-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ivy
  :init
  ;; Windows fix for ag
  ;; Warning: ag does not work with ivy occur and wgrep under Windows, use ripgrep
  (setq counsel-ag-base-command "ag --vimgrep --nocolor --nogroup %s")
  :config
  (ivy-mode t))

(use-package smex)
;;;END: User Interface

;;;BEGIN: Keyboard Shortcuts
(defvar leet-leader-key "SPC"
  "Leet Evil leader key.")

(use-package general
  :config
  (setq general-default-keymaps 'normal)

  (general-define-key "zx" #'kill-buffer)

  (general-define-key "M-x" #'counsel-M-x)

  (general-define-key :keymaps '(insert emacs)
  		      "<C-SPC>" #'company-complete-common)

  (general-define-key :keymaps 'company-active-map
		      "C-j" #'company-select-next
		      "C-k" #'company-select-previous
		      "C-SPC" #'company-complete-common)

  (general-define-key :keymaps 'ivy-minibuffer-map
		      "C-j" #'ivy-next-line
		      "C-k" #'ivy-previous-line)

  ;; Leader keybindings
  (general-define-key :prefix leet-leader-key
		      "SPC" #'(projectile-find-file :which-key "Find Project File")
		      "," #'(switch-to-buffer :which-key "Switch Buffer")
		      "." #'(find-file :which-key "Find File")
		      "TAB" #'(projectile-project-buffers-other-buffer :which-key "Project Other Buffer")
		      "p" #'(projectile-command-map :which-key "Projectile"))

  ;; File keybindings
  (general-define-key :prefix (concat leet-leader-key " f")
                      "" #'(nil :which-key "File")
                      "r" #'(counsel-recentf :which-key "Recent Files")
                      "R" #'(projectile-recentf :which-key "Recent Project Files"))

  ;; Search keybindings
  (general-define-key :prefix (concat leet-leader-key " s")
		      "" #'(nil :which-key "Search")
		      "r" #'(counsel-rg :which-key "rg")
		      "a" #'(counsel-ag :which-key "ag")
		      "c" #'(evil-ex-nohighlight :which-key "Clear Search Highlight")
		      "s" #'(swiper :which-key "Swiper"))

  ;; Git keybindings
  (general-define-key :prefix (concat leet-leader-key " g")
		      "" #'(nil :which-key "Git")
		      "b" #'(magit-blame :which-key "Blame")
		      "s" #'(magit-status :which-key "Status"))

  (general-define-key :keymaps '(magit-blame-mode-map)
  		      "n" #'magit-blame-next-chunk
  		      "p" #'magit-blame-previous-chunk
  		      "q" #'magit-blame-quit)

  ;; Code keybindings
  (general-define-key :prefix (concat leet-leader-key " c")
		      "" #'(nil :which-key "Code"))

  (general-define-key :prefix (concat leet-leader-key " c e")
		      "" #'(nil :which-key "Errors")
		      "l" #'(flycheck-list-errors :which-key "List")
		      "n" #'(flycheck-next-error :which-key "Next")
		      "p" #'(flycheck-previous-error :which-key "Previous"))
  ;; Leet keybindings
  (general-define-key :prefix (concat leet-leader-key " L")
                      "" #'(nil :which-key "Leet"))

  (general-define-key :prefix (concat leet-leader-key " L")
                      "a" #'(borg-assimilate :which-key "Borg Assimilate")
                      "d" #'(epkg-describe-package :which-key "Describe Package")))

(use-package which-key
  :config
  (which-key-mode t))
;;;END: Keyboard Shortcuts

;;;BEGIN: Project Management
(use-package projectile
  :init
  (setq projectile-enable-caching (not noninteractive)
	projectile-require-project-root nil)
  :config
  (add-to-list 'projectile-globally-ignored-directories '"node_modules")
  (add-to-list 'projectile-globally-ignored-directories (expand-file-name "var" user-emacs-directory))
  (add-to-list 'projectile-globally-ignored-directories (expand-file-name "lib" user-emacs-directory))
  (projectile-mode t))

(use-package counsel-projectile
  :config
  (add-hook 'projectile-mode-hook (counsel-projectile-on)))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t)
  (add-hook 'ivy-occur-mode-hook (require 'wgrep)))
;;;END: Project Management

;;;BEGIN: Git Integration
(use-package magit
  :defer t
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-submodules
                          'magit-insert-unpulled-from-upstream))

(use-package git-gutter-fringe
  :config
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX....")
  (add-hook 'after-init-hook #'global-git-gutter-mode))
;;;END: Git Integration

;;;BEGIN: Javascript Development
(use-package web-mode
  :mode "\\.html$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq js-switch-indent-offset 2
	js2-indent-switch-body t))

(use-package js2-refactor
  :defer t
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package tern
  :load-path "lib/tern/emacs"
  :commands tern-mode
  :init (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :defer t
  :init (add-to-list 'company-backends #'company-tern))
;;;END: Javascript Development

