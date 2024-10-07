;;; ftconfig.el --- Filetype-specific configs -*- no-byte-compile: t; lexical-binding: t; -*-

(defun my/set-default-colorcolumn (length)
  (setq display-fill-column-indicator-column length))

(defun my/python-setup ()
  (display-fill-column-indicator-mode 1)
  (my/set-default-colorcolumn 88))

(defun my/js-setup ()
  (display-fill-column-indicator-mode 1)
  (my/set-default-colorcolumn 120))

(add-hook 'python-mode-hook #'my/python-setup)
(add-hook 'js-mode-hook #'my/js-setup)


;; Markdown setup
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  ;; MultiMarkdown package must be installed
  :init (setq markdown-command "multimarkdown"))
