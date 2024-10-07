;;; music.el --- Because why not -*- no-byte-compile: t; lexical-binding: t; -*-

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
