;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)

(pixel-scroll-precision-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;; Themes
;; (setq modus-themes-bold-constructs t
;;       modus-themes-italic-constructs t
;;       modus-themes-mode-line '(3d)
;;       modus-themes-region nil
;;       modus-themes-fringes nil
;;       modus-themes-paren-match '(bold intense)
;;       modus-themes-hl-line '(accented)
;;       modus-themes-syntax '(yellow-comments)
;;       modus-themes-org-blocks 'tinted-background
;;       modus-themes-scale-headings t
;;       modus-themes-headings '((1 . (rainbow overline background 1.6))
;;                               (2 . (rainbow 1.4))
;;                               (3 . (rainbow bold 1.2))
;;                               (4 . (semilight 1.1))))
;; 
;; (load-theme 'modus-vivendi t)

(use-package kanagawa-themes
  :ensure t
  :config
  (load-theme 'kanagawa-dragon t))

(set-face-attribute 'mode-line nil :height 95)  ;; Smaller modeline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read user' shell env
(require 'exec-path-from-shell)

(dolist (var '("PATH"
               "XDG_CONFIG_HOME"
               "XDG_DATA_HOME"
               "XDG_CACHE_HOME"
               "XDG_STATE_HOME"
               "LANG"
               "NVM_DIR"
               "PYENV_ROOT"
               "NPM_HOME"
               "WGETRC"))
  (add-to-list 'exec-path-from-shell-variables var))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom vars/funcs
(defun my/enable-colorcolumn (length)
  (display-fill-column-indicator-mode 1)
  (setq display-fill-column-indicator-column length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
(setq
 evil-want-keybinding nil) ;; must be declared before Evil and Evil Collection

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :custom
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(use-package evil-surround
  :after evil
  :ensure t
  :defer t
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))
     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))
     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal
(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))

(use-package multi-vterm
  :ensure t
  :config
  (add-hook 'vterm-mode-hook (lambda ()
                               (setq-local evil-insert-state-cursor 'bar)
                               (evil-insert-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package consult
  :ensure t
  :bind (;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark))
  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path
        '("~/Documents/dev" "~/Documents/dev-scratch"))
  (setq projectile-require-project-root nil))

(projectile-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-popupinfo-delay '(0.5 0.2))
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treesitter

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist
   '(python go lua json yaml js-mode html dockerfile css cpp c bash go sql tsx typescript vue))
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP (using Eglot)
(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :custom
  (eglot-report-progress nil)  ; Prevent minibuffer spam
  :config
  ;; Optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  (setq eglot-autoshutdown t)
  :hook
  ((python-ts-mode js-ts-mode typescript-ts-mode c-ts-mode c++-ts-mode) . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-ignored-server-capabilities '(:hoverProvider))

  (add-to-list 'eglot-server-programs
               '(js-ts-mode . ("typescript-language-server" "--stdio"))
               '(typescript-ts-mode . ("typescript-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("ccls"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP (using LSP-mode)
;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (defun my/lsp-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))
;;   (setq lsp-completion-provider :none)
;;   (setq lsp-keymap-prefix "C-l")
;;   (setq lsp-diagnostic-provider :flymake)
;;   (setq lsp-keep-workspace-alive nil)
;;   :commands (lsp lsp-deferred)
;;   :custom (read-process-output-max (* 1024 1024))
;;   :hook
;;   (python-mode . lsp-deferred)
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   (lsp-completion-mode . my/lsp-setup-completion))
;; 
;; (use-package lsp-ui
;;   :ensure t
;;   :hook (lsp-mode . lsp-ui-mode))
;; 
;; (use-package flycheck
;;   :ensure t
;;   :custom (flycheck-display-errors-delay .3)
;;   :hook (lsp-mode . flycheck-mode)
;;   :bind (:map flycheck-mode-map
;;               ("M-n" . flycheck-next-error)
;;               ("M-p" . flycheck-previous-error)))
;; 
;; (use-package lsp-pyright
;;   :ensure t
;;   :custom (lsp-pyright-langserver-command "pyright")
;;   :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code formatter (with apheleia)
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))
(setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
(setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
(use-package magit
  :ensure t
  :after evil
  :defer t
  :init
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filetype-specific configs
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  ;; MultiMarkdown package must be installed
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode . visual-line-mode))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Python setup
(add-hook 'python-mode-hook
          #'(lambda ()
              (my/enable-colorcolumn 88)))
(use-package poetry
  :ensure t)

(add-hook 'js-mode-hook
          #'(lambda () (my/enable-colorcolumn 120)))

(add-hook 'css-mode-hook
          #'(lambda ()
              (setq css-indent-offset 2)
              (my/enable-colorcolumn 120)))

(defun my/c-cpp-setup ()
  (setq c-ts-mode-indent-offset 4)
  (my/enable-colorcolumn 80))

(add-hook 'c-ts-mode-hook #'my/c-cpp-setup)
(add-hook 'c++-ts-mode-hook #'my/c-cpp-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode

(with-temp-buffer (org-mode))

(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-directory "~/Documents/org")
  (setq org-export-with-smart-quotes t)
  :bind
  (:map org-mode-map ("<return>" . org-open-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Music player
(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-player-list '(emms-player-mpd)
        emms-info-functions '(emms-info-mpd)
        emms-player-mpd-server-name "0.0.0.0"
        emms-player-mpd-server-port "6600"))

(setq mpc-host "0.0.0.0:6600")

(defun mpd/update-db ()
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD DB Updated."))

(defun mpd/start-music-daemon ()
  (interactive)
  (mpd/update-db)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Utils.
(use-package sudo-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybinds
(use-package general)
(require 'general)
(general-override-mode)

;; Remove search highlight with Esc
(define-key evil-normal-state-map (kbd "<f5>") 'modus-themes-toggle)
(define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)

(global-set-key (kbd "M-`") 'multi-vterm-dedicated-toggle)

(general-define-key  ;; Global
 :states 'normal
 :keymaps 'override
 :prefix "SPC"
 "SPC" 'consult-buffer
 "RET" 'bookmark-jump)

(general-define-key  ;; Terminal
 :states 'normal
 :keymaps 'override
 :prefix "SPC t"
 "o" 'multi-vterm
 "r" 'multi-vterm-project
 "n" 'multi-vterm-next
 "p" 'multi-vterm-prev
 "v" 'projectile-run-vterm-other-window)

(general-define-key  ;; Files
 :states 'normal
 :keymaps 'override
 :prefix "SPC f"
 "s" 'save-buffer
 "e" 'dired-jump
 "u" 'sudo-edit-find-file
 "x" 'eval-buffer)

(general-define-key ;; Buffer leader
 :states 'normal
 :keymaps 'override
 :prefix "SPC b"
 "n" 'evil-next-buffer
 "p" 'evil-prev-buffer
 "d" 'kill-this-buffer
 "b" 'consult-buffer
 "x" 'delete-window)

(general-define-key  ;; Bookmark
 :states 'normal
 :keymaps 'override
 :prefix "SPC k"
 "s" 'bookmark-set
 "d" 'bookmark-delete
 "j" 'bookmark-jump
 "v" 'bookmark-save
 "x" 'bookmark-delete-all)

(general-define-key  ;; Search
 :states 'normal
 :keymaps 'override
 :prefix "SPC s"
 "f" 'find-file
 "l" 'consult-line)

(general-define-key  ;; Help
 :states 'normal
 :keymaps 'override
 :prefix "SPC h"
 "v" 'describe-variable
 "f" 'describe-function)

(general-define-key  ;; Projectile
 :states 'normal
 :keymaps 'override
 :prefix "SPC p"
 "p" 'projectile-switch-project
 "f" 'projectile-find-file
 "b" 'project-switch-to-buffer
 "g" 'consult-ripgrep)

(general-define-key  ;; Magit
 :states 'normal
 :keymaps 'override
 :prefix "SPC g"
 "d" 'magit-status)

(general-define-key  ;; LSP
 :states 'normal
 :prefix "SPC l"
 "r" 'eglot-rename)

(general-define-key  ;; Music Player
 :states 'normal
 :keymaps 'override
 :prefix "SPC m"
 "s" 'mpd/start-music-daemon
 "d" 'mpd/update-db
 "b" 'emms-smart-browse
 "r" 'emms-player-mpd-update-all-reset-cache
 "c" 'emms-player-mpd-connect
 "n" 'emms-next
 "p" 'emms-previous
 "x" 'emms-shuffle
 "/" 'emms-pause
 ";" 'emms-stop)

(general-define-key
 :states 'visual
 :keymaps 'override
 :prefix "SPC e"
 "r" 'eval-region)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
