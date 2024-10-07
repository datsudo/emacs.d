;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)

(pixel-scroll-precision-mode)

;;;;;;;; Configs ;;;;;;;;
(load-file (concat minimal-emacs-user-directory "theme.el"))
(load-file (concat minimal-emacs-user-directory "evil.el"))
(load-file (concat minimal-emacs-user-directory "terminal.el"))
(load-file (concat minimal-emacs-user-directory "navigation.el"))
(load-file (concat minimal-emacs-user-directory "projectile.el"))
(load-file (concat minimal-emacs-user-directory "completion.el"))
(load-file (concat minimal-emacs-user-directory "lsp.el"))
(load-file (concat minimal-emacs-user-directory "magit.el"))
(load-file (concat minimal-emacs-user-directory "keybinds.el"))
(load-file (concat minimal-emacs-user-directory "ftconfig.el"))
