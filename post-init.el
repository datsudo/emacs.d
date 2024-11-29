;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-


(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)

(pixel-scroll-precision-mode)

;; Install straight.el
(straight-use-package 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts

(defun my/configure-font ()
  (set-fontset-font t 'symbol "Noto Color Emoji")
  (set-face-attribute 'variable-pitch nil :font "Atkinson Hyperlegible-15")
  (dolist (face '(default fixed-pitch))
    (set-face-attribute `,face nil :font "Iosevmata Nerd Font SemiBold-12")))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (my/configure-font)))
  (my/configure-font))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes

;; The baked-in Modus themes
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

;; (use-package kaolin-themes
;;   :init
;;   (setq kaolin-themes-italic-comments t
;;         kaolin-themes-hl-line-colored t
;;         kaolin-themes-git-gutter-solid t)
;;   :config
;;   (load-theme 'kaolin-valley-dark t))

(use-package ef-themes
  :init
  (setq ef-themes-to-toggle '(ef-cherie ef-duo-light))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui nil)
  (setq ef-elea-dark-palette-overrides '((bg-main "#14161B"))
        ef-cherie-palette-overrides '((bg-main "#14161B"))
        ef-deuteranopia-dark-palette-overrides '((bg-main "#14161B"))
        ef-bio-palette-overrides '((bg-main "#14161B"))
        ef-cherie-palette-overrides '((bg-main "#1A1D23")
                                      (bg-hl-line "#301726")
                                      (bg-region "#1C3659")
                                      (bg-mode-line "#3F363A")  ;; 2C3B4F
                                      (border "#322A2D")))
  :config
  (load-theme 'ef-cherie t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "FiraCode Nerd Font Med"))

(use-package mini-echo
  :init (mini-echo-mode 1))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-height 15
;;         doom-modeline-bar-width 0
;;         doom-modeline-icon t
;;         doom-modeline-major-mode-icon nil
;;         doom-modeline-hud nil
;;         doom-modeline-buffer-file-name-style 'relative-from-project
;;         doom-modeline-buffer-state-icon nil
;;         doom-modeline-minor-modes nil
;;         doom-modeline-indent-info t
;;         doom-modeline-workspace-name t))

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
               "TYPST_ROOT"
               "MPD_HOST"
               "WGETRC"))
  (add-to-list 'exec-path-from-shell-variables var))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

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

(defun my/roam-template (note-category additional-config)
  (concat "#+TITLE: ${title}\n"
          "#+DATE: %U\n"
          (concat "#+FILETAGS: :"
                  note-category
                  ":\n\n")))

(defun my/css-setup ()
  (setq css-indent-offset 2)
  (setq fill-column 120))

(defun my/go-setup ()
  (setq go-ts-mode-indent-offset 4))

(defun my/c-cpp-setup ()
  (setq c-ts-mode-indent-offset 4))

(defun my/create-tab(name)
  (condition-case nil
      (unless (equal (alist-get 'name (tab-bar--current-tab))
                     name)
        (tab-bar-rename-tab-by-name name name))
    (error (tab-bar-new-tab)
           (tab-bar-rename-tab name))))

(defun my/tab-bar-exists (name)
  (member name
          (mapcar #'(lambda (tab) (alist-get 'name tab))
                  (tab-bar-tabs))))

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
;; Frame/tabs handling

(tab-bar-mode 1)

;; (use-package beframe
;;   :init
;;   (setq beframe-global-buffers '("*scratch*"))
;;   (setq beframe-create-frame-scratch-buffer nil)
;;   :config
;;   (beframe-mode 1))

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
        '("~/Documents/dev" "~/Documents/dev-scratch"))
  (setq projectile-require-project-root nil))

(projectile-mode +1)

;; For Go projects
(defun my/project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'my/project-find-go-module)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
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
   '(python lua json yaml js-mode html dockerfile cpp c bash go sql tsx typescript vue))
  (global-treesit-auto-mode))

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
  :hook ((python-ts-mode) . apheleia-mode))

(with-eval-after-load 'apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets

(use-package yasnippet
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
(use-package magit
  :ensure t
  :after evil
  :defer t
  :init
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode

(with-temp-buffer (org-mode))

(use-package org
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-directory "~/Documents/org"
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
                        :font "Atkinson Hyperlegible"
                        :weight 'bold
                        :height (cdr face)))

  (dolist (face '(org-formula
                  org-checkbox))
    (set-face-attribute `,face nil :inherit 'fixed-pitch :height 140))

  (dolist (face '(org-code
                  org-table
                  org-verbatim))
    (set-face-attribute `,face nil :inherit '(shadow fixed-pitch) :height 150))

  (dolist (face '(org-special-keyword org-meta-line))
    (set-face-attribute `,face nil :inherit '(font-lock-comment-face fixed-pitch) :height 140))

  (variable-pitch-mode 1))

;; (defun my/org-misc-setup ()
;;   (let* ((variable-tuple
;;           (cond
;;            ((x-list-fonts "Atkinson Hyperlegible") '(:family "Atkinson Hyperlegible-15"))))
;;          (fixed-tuple
;;           (cond
;;            ((x-list-fonts "Iosevmata Nerd Font") '(:family "Iosevmata Nerd Font-14"))))
;;          (headline `(:inherit default :weight bold)))
;;     (custom-theme-set-faces
;;      'user
;;      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
;;      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
;;      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
;;      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;      `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.0))))
;;      `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;      `(org-document-title ((t (,@headline ,@variable-tuple :height 1.6 :underline nil))))
;;      `(variable-pitch     ((t ,@variable-tuple)))
;;      `(fixed-pitch        ((t ,@fixed-tuple)))))
;;   (corfu-mode -1)
;;   (electric-pair-mode -1))
;; 
(add-hook 'org-mode-hook 'my/org-misc-setup)

(use-package org-superstar
  :ensure t
  :hook
  (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s
        org-indent-mode-turns-on-hiding-stars nil))

;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (add-hook 'org-mode-hook 'my/org-misc-setup)

(with-eval-after-load "org"
  (require 'org-tempo))

;; Org Roam
(use-package org-roam
  :ensure t
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates `(("r" "Resource Note" plain "%?"
                                 :if-new (file+head "resources/${slug}.org"
                                                    ,(my/roam-template "resource" ""))
                                 :immediate-finish t
                                 :unnarrowed t)
                                ("n" "Note" plain "%?"
                                 :if-new (file+head "notes/${slug}.org"
                                                    ,(my/roam-template "draft" ""))
                                 :immediate-finish t
                                 :unnarrowed t)))
  (org-roam-node-display-template (concat "${title:100}  "
                                          (propertize "${tags:50}" 'face 'org-tag))))

(setq org-bookmark-names-plist nil)

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-i") #'org-roam-node-insert)))

(add-hook 'org-mode-hook 'org-modern-mode)

;; Search org roam contents using ripgrep
(defun my/org-roam-rg-search ()
  (interactive)
  (let ((consult-ripgrep-command
         "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

;; Add UI for graphs and note preview
(use-package websocket :after org-roam)
(use-package org-roam-ui
  :after org-roam
  :defer t
  :custom
  (org-roam-ui-update-on-save t))

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

(use-package poetry :ensure t :defer t)

;; Typst mode
(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode"
                    :files (:defaults "*.el"))
  :config
  (setq typst-ts-mode-indent-offset 2))
(with-temp-buffer (typst-ts-mode))

(use-package yaml-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)

;; Python setup
(add-hook 'python-ts-mode-hook #'(lambda () (setq fill-column 88)))

;; Web dev setup
(add-hook 'js-ts-mode-hook #'(lambda () (setq fill-column 120)))
(add-hook 'css-ts-mode-hook #'my/css-setup)

;; Go setup
(add-hook 'go-ts-mode-hook #'my/go-setup)

;; C-C++ setup
(add-hook 'c-ts-mode-hook #'my/c-cpp-setup)
(add-hook 'c++-ts-mode-hook #'my/c-cpp-setup)

;; Lua setup
(use-package lua-mode
  :init
  (setq lua-indent-level 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;; Enable visual line mode in some modes
(my/add-to-multiple-hooks 'visual-line-mode
                          '(org-mode-hook
                            eldoc-mode-hook
                            typst-ts-mode-hook
                            lisp-interaction-mode-hook
                            special-mode-hook))

(my/add-to-multiple-hooks 'display-fill-column-indicator-mode
                          '(c-ts-mode-hook
                            c++-ts-mode-hook
                            python-ts-mode-hook
                            go-ts-mode-hook
                            js-ts-mode-hook
                            css-ts-mode-hook))

(use-package web-mode
  :ensure t
  :defer t
  :init
  (setq web-mode-enable-current-element-highlight t
        web-mode-enable-auto-pairing t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 4)
  :mode
  (("\\.php\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.css\\'" . web-mode)))

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
  (js-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (typst-ts-mode . eglot-ensure)
  (lua-ts-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-ignored-server-capabilities '(:hoverProvider))

  (add-to-list 'eglot-server-programs
               '(js-ts-mode . ("typescript-language-server" "--stdio"))
               '(typescript-ts-mode . ("typescript-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '(lua-mode . ("lua-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("ccls"))))


(with-eval-after-load 'eglot
  (with-eval-after-load 'typst-ts-mode
    (add-to-list 'eglot-server-programs
                 `((typst-ts-mode) .
                   ,(eglot-alternatives `(,typst-ts-lsp-download-path
                                          "tinymist"
                                          "typst-lsp"))))))

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
        emms-player-mpd-server-name "127.0.0.1"))

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
;; PDF tools

(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  :config
  (setq pdf-view-midnight-colors '("#ffffff" . "#1A1D23")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSS FEED

(use-package elfeed
  :config
  (setq elfeed-feeds '(("https://hackernewsrss.com/feed.xml" hn-front))
        elfeed-search-filter "@1-week-ago "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Utils.
(use-package sudo-edit)

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

;; For ligatures
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                         (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode t))

(defun my/music-tab ()
  (interactive)
  (if (equal (my/tab-bar-exists "music") nil)
      (funcall (lambda ()
                 (my/create-tab "music")
                 (tab-switch "music")
                 (emms-smart-browse)
                 (emms-browse-by-album)))
      (tab-switch "music")))

(defun my/files-tab ()
  (interactive)
  (if (equal (my/tab-bar-exists "files") nil)
      (let ((buf-dir (file-name-directory (buffer-file-name))))
        (funcall (lambda ()
                   (my/create-tab "files")
                   (tab-switch "files")
                   (dired buf-dir))))
      (tab-switch "files")))

(defun my/terms-tab ()
  (interactive)
  (if (equal (my/tab-bar-exists "terminal") nil)
      (funcall (lambda ()
                 (my/create-tab "terminal")
                 (funcall (lambda () (tab-switch "terminal") (multi-vterm)))))
      (funcall (lambda () (tab-switch "terminal") (multi-vterm)))))

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

;; Beframe
;; (define-key global-map (kbd "C-c b") #'beframe-prefix-map)

;; Compile key
(global-set-key [f5] (lambda ()
                       (interactive)
                       (let ((current-prefix-arg '(4)))
                         (call-interactively 'compile))
                       (evil-window-down 0)))

(global-set-key [f6] 'ef-themes-toggle)

(global-set-key (kbd "M-`") 'multi-vterm-dedicated-toggle)

(general-define-key  ;; Global
 :states 'normal
 :keymaps 'override
 :prefix "SPC"
 "SPC" 'consult-buffer
 "x" 'execute-extended-command
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

(general-define-key  ;; Org
 :states 'normal
 :keymaps 'override
 :prefix "SPC n"
 "f" 'org-roam-node-find
 "r" 'my/org-roam-rg-search
 "o" 'org-open-at-point
 "b" 'org-mark-ring-goto) 

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
 "b" 'my/music-tab
 "r" 'emms-player-mpd-update-all-reset-cache
 "c" 'emms-player-mpd-connect
 "n" 'emms-next
 "p" 'emms-previous
 "x" 'emms-shuffle
 "/" 'emms-pause
 ";" 'emms-stop)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC d"
 "o" 'elfeed
 "u" 'elfeed-update)

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
