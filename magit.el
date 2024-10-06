;;; magit.el --- Magit Config -*- no-byte-compile: t; lexical-binding: t; -*-


(use-package magit
  :ensure t
  :after evil
  :defer t
  :init
  (evil-collection-init))
