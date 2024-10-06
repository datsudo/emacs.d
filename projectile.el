;;; projectile.el --- Projectile-related Config -*- no-byte-compile: t; lexical-binding: t; -*-


(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/Documents/dev" "~/Documents/dev-scratch"))
  (setq projectile-require-project-root nil))

(projectile-mode +1)
