;;; keybinds.el --- Custom Keybindings -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package general)
(require 'general)
(general-override-mode)

;; Remove search highlight with Esc
(define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)
(global-set-key (kbd "M-`") 'vterm-toggle-cd)

(general-define-key  ;; Global
  :states 'normal
  :keymaps 'override
  :prefix "SPC"
  "SPC" 'consult-buffer)

(general-define-key  ;; Terminal
  :states 'normal
  :keymaps 'override
  :prefix "SPC t"
  "o" 'vterm
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

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
