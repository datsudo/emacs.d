;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)

(pixel-scroll-precision-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
(defun my/configure-font ()
  (dolist (face '(default fixed-pitch))
    (set-face-attribute `,face nil :font "Iosevmata-12")))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (my/configure-font)))
  (my/configure-font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Mode line

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15
        doom-modeline-bar-width 0
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-hud nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-state-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-indent-info t
        doom-modeline-workspace-name t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom vars/funcs
(defun my/add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun my/new-project-tab ()
  (interactive)
  (other-tab-prefix)
  (projectile-switch-project)
  (tab-rename (projectile-project-name)))

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
;; Frame/tabs handling
(tab-bar-mode 1)

;; Bufferlo
(use-package bufferlo
  :ensure t
  :init
  (setq tab-bar-new-tab-choice "*scratch*")
  :config
  (bufferlo-mode 1))

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

;; Bufferlo + consult integration
(defvar my-consult--source-buffer
  `(:name "All Buffers"
          :narrow   ?a
          :hidden   t
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :items ,(lambda () (consult--buffer-query
                              :sort 'visibility
                              :as #'buffer-name)))
  "All buffer candidate source for `consult-buffer'.")

(defvar my-consult--source-local-buffer
  `(:name nil
          :narrow   ?b
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default  t
          :items ,(lambda () (consult--buffer-query
                              :predicate #'bufferlo-local-buffer-p
                              :sort 'visibility
                              :as #'buffer-name)))
  "Local buffer candidate source for `consult-buffer'.")

(setq consult-buffer-sources '(consult--source-hidden-buffer
                               my-consult--source-buffer
                               my-consult--source-local-buffer
                               ;; ... other sources ...
                               ))

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
        '("c:/Users/datsudo/git"))
  (setq projectile-require-project-root nil))

(projectile-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  ;; (corfu-cycle t)
  ;; (corfu-auto t)
  ;; (corfu-auto-delay 0)
  ;; (corfu-auto-prefix 1)
  ;; (corfu-popupinfo-delay '(0.5 0.2))
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :init
  (global-corfu-mode))

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
;; Snippets
(use-package yasnippet
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
(with-temp-buffer (org-mode))

(use-package org
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-directory "c:/Users/datsudo/Documents/notes"
        org-export-with-smart-quotes t
        org-list-indent-offset 2
        org-hide-emphasis-markers t
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0))

;; Beautification
(defun my/org-misc-setup ()
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (dolist (face '((org-document-title . 1.5)
                  (org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)))
    (set-face-attribute (car face) nil
                        :font "Iosevmata"
                        :weight 'bold
                        :height (cdr face))))

(add-hook 'org-mode-hook 'my/org-misc-setup)

(use-package org-superstar
  :ensure t
  :hook
  (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s
        org-indent-mode-turns-on-hiding-stars nil))

(with-eval-after-load "org"
  (require 'org-tempo))

(setq org-bookmark-names-plist nil)

(use-package olivetti
  :hook
  (org-mode . olivetti-mode))

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filetype-specific configs
(use-package markdown-mode
  :ensure t
  :defer t
  ;; :mode ("README\\.md\\'" . gfm-mode)
  ;; MultiMarkdown package must be installed
  ;; :init (setq markdown-command "multimarkdown")
  )

(add-hook 'markdown-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Utils.
(use-package persistent-scratch)

(load-file "~/.emacs.d/lib/dired-plus.el")
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)
(setq dired-listing-switches "-alh --group-directories-first")  ;; display "human-readable" file size

(setq-default ibuffer-saved-filter-groups
              `(("Default"
                 ;; I create a group call Dired, which contains all buffer in dired-mode
                 ("Dired" (mode . dired-mode))
                 ("Temporary" (name . "\*.*\*")))))
(add-hook 'ibuffer-mode-hook #'(lambda () (ibuffer-switch-to-saved-filter-groups "Default")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybinds
(use-package general)
(require 'general)
(general-override-mode)

;; Remove search highlight with Esc
(define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Kill dired immediately
(define-key dired-mode-map "q" 'kill-this-buffer)

(general-define-key  ;; Global
 :states 'normal
 :keymaps 'override
 :prefix "SPC"
 "SPC" 'consult-buffer
 "x" 'execute-extended-command
 "RET" 'bookmark-jump)

(general-define-key  ;; Files
 :states 'normal
 :keymaps 'override
 :prefix "SPC f"
 "s" 'save-buffer
 "e" 'dired-jump
 "u" 'sudo-edit-find-file
 "x" 'eval-buffer
 "c" '(lambda ()
        (interactive)
        (let ((current-prefix-arg '(4)))
          (call-interactively 'compile))
        (evil-window-down 1)))

(general-define-key ;; Buffer leader
 :states 'normal
 :keymaps 'override
 :prefix "SPC b"
 "n" 'evil-next-buffer
 "p" 'evil-prev-buffer
 "d" 'kill-this-buffer
 "b" 'consult-buffer
 "v" 'revert-buffer
 "i" 'ibuffer
 "x" 'delete-window)

(general-define-key ;; Tab management
 :states 'normal
 :keymaps 'override
 :prefix "SPC TAB"
 "TAB" 'tab-switch
 "r" 'tab-rename
 "n" 'tab-new
 "d" 'tab-close)

(general-define-key ;; Window management
 :states 'normal
 :keymaps 'override
 :prefix "SPC w"
 "s" 'evil-window-split
 "v" 'evil-window-vsplit
 "h" 'evil-window-left
 "j" 'evil-window-down
 "k" 'evil-window-up
 "l" 'evil-window-right
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
 ;; "p" 'projectile-switch-project
 "f" 'projectile-find-file
 "b" 'project-switch-to-buffer
 "g" 'consult-ripgrep
 "p" 'my/new-project-tab  ;; originally "t"
 "x" 'projectile-kill-buffers)

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
