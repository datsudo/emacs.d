;;; terminal.el --- Terminal Config (specifically VTerm) -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))
;;(add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode -1)))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :defer t
  :config
  (setq vterm-toggle-fullscreen-p nil))

(with-eval-after-load 'vterm
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))
