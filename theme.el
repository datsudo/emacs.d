;;; theme.el --- Theme Config -*- no-byte-compile: t; lexical-binding: t; -*-

(load-theme 'modus-vivendi t)

(setq modus-themes-vivendi-color-overrides '((bg-main . "#080808")))

(modus-themes-with-colors
  ;; active: bg-inactive
  ;; inactive: #000000
  (set-face-attribute 'mode-line nil
                      :background bg-inactive
                      :foreground fg-main
                      :box bg-alt)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#000000"
                      :foreground bg-inactive
                      :box bg-dim))
