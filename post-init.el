;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)

(pixel-scroll-precision-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
(set-face-attribute 'mode-line nil :height 110)  ;; Smaller modeline

;; Themes
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-mode-line '(3d)
      modus-themes-region nil
      modus-themes-fringes nil
      modus-themes-paren-match '(bold intense)
      modus-themes-hl-line '(accented)
      modus-themes-syntax '(yellow-comments)
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-headings '((1 . (rainbow overline background 1.6))
                              (2 . (rainbow 1.4))
                              (3 . (rainbow bold 1.2))
                              (4 . (semilight 1.1))))

(load-theme 'modus-vivendi t)

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
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

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
  (corfu-auto t)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
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
  (eglot-autoshutdown t)

  :config
  ;; Optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil))

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
  :init (setq markdown-command "multimarkdown"))

(add-hook 'python-mode-hook
          #'(lambda ()
             (my/enable-colorcolumn 88)))

(add-hook 'js-ts-mode-hook
          #'(lambda () (my/enable-colorcolumn 120)))

(add-hook 'css-mode-hook
          #'(lambda ()
             (setq css-indent-offset 2)
             (my/enable-colorcolumn 120)))

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
;; Keybinds
(use-package general)
(require 'general)
(general-override-mode)

;; Remove search highlight with Esc
(define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)
(global-set-key (kbd "M-`") 'multi-vterm-dedicated-toggle)

(general-define-key  ;; Global
  :states 'normal
  :keymaps 'override
  :prefix "SPC"
  "SPC" 'consult-buffer)

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
 "d" 'consult-flymake)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC m"
 "s" 'mpd/start-music-daemon
 "d" 'mpd/update-db
 "b" 'emms-smart-browse
 "r" 'emms-player-mpd-update-all-reset-cache
 "n" 'emms-next
 "p" 'emms-previous
 "/" 'emms-pause
 ";" 'emms-stop)  ;; Music Player

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
