;;; init.el --- leet-init-file                    -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-save-file (expand-file-name "var/recentf" user-emacs-directory))
  (recentf-mode))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "var/places" user-emacs-directory))
  (save-place-mode))

(use-package simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

(progn ;     General
  (defalias 'yes-or-no-p 'y-or-n-p))

(progn ;     UI
  (set-face-attribute 'default nil :font "Source Code Pro-16")
  (toggle-frame-maximized)
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

(progn ;     Code
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

;;;BEGIN: custom functions
(defun leet-project-root ()
  "Get the path to the root of your project."
  (let ((projectile-require-project-root))
    (ignore-errors (projectile-project-root))))

(defun leet-new-buffer ()
  "Create new buffer and set it as current."
  (interactive)
  (let ((buffer (generate-new-buffer "*untitled*")))
    (when buffer
      (display-buffer buffer '(display-buffer-same-window)))))
;;;END: custom functions

;;;BEGIN: evil
(use-package evil
  :config
  ;; (evil-select-search-module 'evil-search-module 'evil-search) ;; emacs hangs on searching in html files
  (evil-mode t))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode))

(use-package evil-magit
  :after magit)

(use-package evil-anzu
  :after anzu)

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode t))
;;;END: evil

;;;BEGIN: Keyboard Shortcuts
(defvar leet-leader-key "SPC"
  "Leet Evil leader key.")
(defvar leet-local-leader-key "m"
  "Leet Evil local leader key.")

(use-package general
  :config
  (setq general-default-keymaps 'normal)

  (general-define-key :keymaps '(normal emacs motion)
                      "TAB" #'evilmi-jump-items)

  (general-define-key :keymaps '(insert emacs)
                      "<C-SPC>" #'company-complete-common)

  (general-define-key :keymaps 'company-active-map
                      "C-j" #'company-select-next
                      "C-k" #'company-select-previous
                      "C-SPC" #'company-complete-common)

  (general-define-key "zx" #'kill-this-buffer)
  (general-define-key "zX" #'kill-buffer-and-window)

  ;; Leader keybindings
  (general-define-key :prefix leet-leader-key
                      "," #'(switch-to-buffer :which-key "Switch Buffer")
                      "." #'(find-file :which-key "Find File"))

  ;; Applications Keybindings
  (general-define-key :prefix (concat leet-leader-key " a")
                      "" #'(nil :which-key "Applications"))

  ;; Buffer Keybindings
  (general-define-key :prefix (concat leet-leader-key " b")
                      "" #'(nil :which-key "Buffers")
                      "b" #'(ibuffer :which-key "List")
                      "n" #'(leet-new-buffer :which-key "New"))

  ;; File keybindings
  (general-define-key :prefix (concat leet-leader-key " f")
                      "" #'(nil :which-key "File"))

  ;; Search keybindings
  (general-define-key :prefix (concat leet-leader-key " s")
                      "" #'(nil :which-key "Search")
                      "c" #'(evil-ex-nohighlight :which-key "Clear Search"))

  ;; Git keybindings
  (general-define-key :prefix (concat leet-leader-key " g")
                      "" #'(nil :which-key "Git"))

  ;; Code keybindings
  (general-define-key :prefix (concat leet-leader-key " c")
                      "" #'(nil :which-key "Code"))

  (general-define-key :prefix (concat leet-leader-key " c e")
                      "" #'(nil :which-key "Errors"))

  ;; Leet keybindings
  (general-define-key :prefix (concat leet-leader-key " L")
                      "" #'(nil :which-key "Leet"))

  (general-define-key :prefix (concat leet-leader-key " L")
                      "a" #'(borg-assimilate :which-key "Borg Assimilate")
                      "c" #'(borg-clone :which-key "Borg Clone")
                      "r" #'(borg-remove :which-key "Borg Remove"))

  ;; Window Keybindings
  (evil-define-key 'normal (current-global-map)
    (kbd "C-h") #'evil-window-left
    (kbd "C-j") #'evil-window-down
    (kbd "C-k") #'evil-window-up
    (kbd "C-l") #'evil-window-right)

  ;; Help keybindings
  (general-define-key :prefix (concat leet-leader-key " h")
                      "" #'(nil :which-key "Help"))

  (general-define-key :prefix (concat leet-leader-key " h")
                      "v" #'(counsel-describe-variable :which-key "Describe Variable")
                      "k" #'(describe-key :which-key "Describe Key")
                      "K" #'(general-describe-keybindings :which-key "Describe Keybindings")
                      "f" #'(counsel-describe-function :which-key "Describe Function")
                      "F" #'(counsel-describe-face :which-key "Describe Face")
                      "m" #'(describe-mode :which-key "Describe Mode")
                      "M" #'(describe-minor-mode :which-key "Describe Minor Mode")
                      "a" #'(counsel-apropos :which-key "Apropos")
                      "p" #'(epkg-describe-package :which-key "Describe Package")))

(use-package which-key
  :config
  (which-key-mode t))
;;;END: Keyboard Shortcuts

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
  :general
  (:prefix (concat leet-leader-key " c e")
           "l" #'(flycheck-list-errors :which-key "List"))
  ("] r" #'(flycheck-next-error :which-key "Flycheck Next Error"))
  ("[ r" #'(flycheck-previous-error :which-key "Flycheck Previous Error"))
  :config
  (setq flycheck-check-syntax-automatically #'(mode-enabled save))
  :init
  (add-hook 'flycheck-mode-hook #'leet-init-flycheck-eslint)
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package undo-tree
  :general
  (:prefix (concat leet-leader-key " a")
           "u" #'(undo-tree-visualize :which-key "Undo Tree"))
  :config
  ;; persistent undo history is known to cause undo history corruption, which
  ;; can be very destructive! So disable it!
  (setq undo-tree-auto-save-history nil))

(use-package avy
  :general
  ("S-SPC" #'(avy-goto-char)))

;;;BEGIN: User Interface
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-one t)
  (set-face-attribute 'doom-modeline-error nil :height 150))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-default-separator 'bar
        powerline-height 30)
  (set-face-attribute 'spaceline-evil-emacs nil :background "#dfdfdf")
  (set-face-attribute 'spaceline-evil-insert nil :background "#99bb66")
  (set-face-attribute 'spaceline-evil-motion nil :background "#ae81ff")
  (set-face-attribute 'spaceline-evil-normal nil :background "#51afef")
  (set-face-attribute 'spaceline-evil-replace nil :background "#da8548")
  (set-face-attribute 'spaceline-evil-visual nil :background "#c678dd")
  (spaceline-toggle-minor-modes-off))

(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (setq spaceline-all-the-icons-separator-type 'none
        spaceline-all-the-icons-hide-long-buffer-path t)
  (set-face-attribute 'mode-line nil :height 150)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-toggle-all-the-icons-time-off)
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-all-the-icons-theme))

(use-package nlinum
  :config
  (add-hook 'after-init-hook #'global-nlinum-mode))

(use-package anzu
  :config
  (set-face-attribute 'anzu-mode-line nil :height 160 :foreground "#51afef")
  (set-face-attribute 'anzu-mode-line-no-match nil :foreground "#bf5150")
  (global-anzu-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ivy
  :general
  ("M-x" #'counsel-M-x)
  (:prefix (concat leet-leader-key " f")
           "r" #'(counsel-recentf :which-key "Recent Files"))
  (:prefix (concat leet-leader-key " s")
           "a" #'(counsel-ag :which-key "The Silver Searcher")
           "s" #'(swiper :which-key "Swiper"))
  (:keymaps 'ivy-minibuffer-map
            "C-j" #'ivy-next-line
            "C-k" #'ivy-previous-line)
  :init
  ;; Windows fix for ag
  ;; Warning: ag does not work with ivy occur and wgrep under Windows, use ripgrep
  (setq counsel-ag-base-command "ag --vimgrep --nocolor --nogroup %s")
  :config
  (ivy-mode t))

(use-package shackle
  :config
  (setq shackle-default-rule '(:select t))
  (shackle-mode t))
;;;END: User Interface

;;;BEGIN: Project Management
(use-package projectile
  :general
  (:prefix leet-leader-key
           "SPC" #'(projectile-find-file :which-key "Find Project File")
           "TAB" #'(projectile-project-buffers-other-buffer :which-key "Project Other Buffer")
           "p" #'(projectile-command-map :which-key "Projectile"))
  (:prefix (concat leet-leader-key " f")
           "R" #'(projectile-recentf :which-key "Recent Project Files"))
  :init
  (setq projectile-enable-caching (not noninteractive)
        projectile-require-project-root nil)
  :config
  (setq projectile-cache-file (expand-file-name "var/projectile.cache" user-emacs-directory)
        projectile-known-projects-file (expand-file-name "var/projectile-known-projects.eld" user-emacs-directory))
  (add-to-list 'projectile-globally-ignored-directories '"node_modules")
  (add-to-list 'projectile-globally-ignored-directories (expand-file-name "var" user-emacs-directory))
  (add-to-list 'projectile-globally-ignored-directories (expand-file-name "lib" user-emacs-directory))
  (projectile-mode))

(use-package counsel-projectile
  :config
  (add-hook 'projectile-mode-hook (counsel-projectile-on)))

(use-package rg
  :general
  (:prefix (concat leet-leader-key " s")
           "r" #'(nil :which-key "Ripgrep"))
  (:prefix (concat leet-leader-key " s r")
           "r" #'(rg :which-key "Directory")
           "d" #'(rg-dwim :which-key "DWIM")
           "l" #'(rg-literal :which-key "Literal")
           "p" #'(rg-project :which-key "Project")
           "s" #'(rg-list-searches :which-key "List Saved Searches")
           "c" #'(counsel-rg :which-key "Counsel"))
  :init
  (add-hook 'rg-mode-hook 'wgrep-ag-setup))

(use-package wgrep
  :after ivy-occur
  :config
  (setq wgrep-auto-save-buffer t))

(use-package neotree
  :general
  (:prefix (concat leet-leader-key " a")
           "n" #'(nil :which-key "Neotree"))
  (:prefix (concat leet-leader-key " a n")
           "n" #'(neotree-toggle :which-key "Toggle")
           "p" #'(neotree-projectile-action :which-key "Project")
           "d" #'(neotree-dir :which-key "Directory"))
  (:keymaps '(neotree-mode-map) :states '(normal)
            "q" #'neotree-hide
            "I" #'neotree-hidden-file-toggle
            "z" #'neotree-stretch-toggle
            "R" #'neotree-refresh
            "m" #'neotree-rename-node
            "c" #'neotree-create-node
            "d" #'neotree-delete-node

            "s" #'neotree-enter-vertical-split
            "S" #'neotree-enter-horizontal-split

            "RET" #'neotree-enter))
;;;END: Project Management

;;;BEGIN: Git Integration
(use-package magit
  :general
  (:prefix (concat leet-leader-key " g")
           "b" #'(magit-blame :which-key "Blame")
           "s" #'(magit-status :which-key "Status"))
  (:keymaps '(magit-blame-mode-map)
            "n" #'magit-blame-next-chunk
            "p" #'magit-blame-previous-chunk
            "q" #'magit-blame-quit)
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
                          'magit-insert-unpulled-from-upstream)
  :init
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package git-gutter-fringe
  :demand t
  :general
  ("] c" #'(git-gutter:next-hunk :which-key "Next Hunk"))
  ("[ c" #'(git-gutter:previous-hunk :which-key "Previous Hunk"))
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

(use-package git-link
  :general
  (:prefix (concat leet-leader-key " g")
           "l" #'(nil :which-key "Link")
           "l l" #'(git-link :which-key "Line")
           "l c" #'(git-link-commit :which-key "Commit")
           "l h" #'(git-link-homepage :which-key "Homepage"))
  :config
  (setq git-link-open-in-browser t))

(use-package git-timemachine
  :general
  (:prefix (concat leet-leader-key " g")
           "t" #'(git-timemachine-toggle :which-key "Timemachine"))
  (:keymaps '(git-timemachine-mode-map)
            "q" #'git-timemachine-quit
            "p" #'git-timemachine-show-previous-revision
            "n" #'git-timemachine-show-next-revision
            "g" #'git-timemachine-show-nth-revision
            "w" #'git-timemachine-kill-abbreviated-revision
            "W" #'git-timemachine-kill-revision
            "b" #'git-timemachine-blame)
  :config
  ;; https://github.com/emacs-evil/evil/issues/511
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))
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
  (setq js-switch-indent-offset 2)
  (setq js2-basic-offset 2)
  (setq js2-indent-switch-body t))

(use-package js2-refactor
  :defer t
  :general
  (:keymaps '(js2-mode-map) :states '(normal)
    :prefix leet-local-leader-key
    "r" #'(nil :which-key "Refactor")
    "r k" #'(js2r-kill :which-key "Kill")
    "r r" #'(js2r-rename-var :which-key "Rename")
    "r l" #'(js2r-log-this :which-key "Log This"))
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode))
;;;END: Javascript Development

;;; init.el ends here
