;;; spot4e.el --- Control Spotify playback with Helm.
;; Copyright 2018 Charlie Baker
;;
;; Author: Charlie Baker <mister.chiply@gmail.com>
;; Maintainer: Charlie Baker <mister.chiply@gmail.
;; URL: https://github.com/chiply/spot4e
;; Version: 0.0.1
;; Package-Requires: helm, json, url

;;; Commentary:
;;
;; Helm interface to Spotify API, which offers search and playback
;; features.
;;
;; This program is mostly OS agnostic, as it controls Spotify
;; _playback_ via the API, rather than interfacing directly with the
;; media player on the user's computer.

;;; Code:

;;; API Reference: https://developer.spotify.com/web-api/
(require 'json)
(require 'helm)
(require 'url)

;;; Code:
(defvar spot4e-client-id "7b036492368d47c492d048aa8aec339b")
(defvar spot4e-client-secret "4deb0f8b549f436a9d46202e701b57b8")
(defvar spot4e-id-secret
      (concat spot4e-client-id ":" spot4e-client-secret))
(defvar spot4e-b64-id-secret
      (base64-encode-string spot4e-id-secret t))
(defvar spot4e-redirect-uri (url-hexify-string "https://spotify.com"))
(defvar spot4e-auth-url-full
      (concat
       "https://accounts.spotify.com/en/authorize"
       "?response_type=code&client_id=" spot4e-client-id
       "&redirect_uri=" spot4e-redirect-uri
       "&scope=" (concat "streaming "
			 "user-read-birthdate "
			 "user-read-email "
			 "user-read-private "
			 "user-read-playback-state ")
       "&show_dialog=" "true"))
(defvar spot4e-token-url "https://accounts.spotify.com/api/token")
(defvar spot4e-search-url "https://api.spotify.com/v1/search")

;; there are synchronicity issues when displaying currently-playing
;; after doing player action (i.e. next, previous), even when using
;; spot4e-message-currently-playing as a callback funtion, so for now
;; force sleep time between changing track and calling
;; spot4e-message-currently-playing to resolve this issue.
(defvar spot4e-wait-time 0.4)
(defvar spot4e-player-url "https://api.spotify.com/v1/me/player")
(defvar spot4e-player-play-url (concat spot4e-player-url "/play"))
(defvar spot4e-currently-playing-url (concat spot4e-player-url "/currently-playing"))


(defun spot4e-authorize ()
  "Obtain access_ and refresh_ tokens for user account."
  (interactive)
  (browse-url spot4e-auth-url-full)
  (setq spot4e-auth-code (read-string "Enter code from URL: "))
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 `(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Authorization" . ,(concat "Basic " spot4e-b64-id-secret))))
	(q-params (concat "?grant_type=" "authorization_code"
			  "&redirect_uri=" spot4e-redirect-uri
			  "&code=" spot4e-auth-code)))
    (setq spot4e-tokens-alist
	  (spot4e-retrieve-url-to-alist-synchronously (concat spot4e-token-url q-params)))
    (setq spot4e-access-token
	  (alist-get 'access_token spot4e-tokens-alist))
    (setq spot4e-refresh-token
	  (alist-get 'refresh_token spot4e-tokens-alist))))


(defun spot4e-refresh ()
  "Obtain access_ and refresh_ tokens for user account."
  (interactive)
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 `(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Authorization" . ,(concat "Basic " spot4e-b64-id-secret))))
	(q-params (concat "?grant_type=" "refresh_token"
			  "&refresh_token=" spot4e-refresh-token)))
    (setq spot4e-refresh-alist
	  (spot4e-retrieve-url-to-alist-synchronously (concat spot4e-token-url q-params)))
    (setq spot4e-access-token
	  (alist-get 'access_token spot4e-refresh-alist))))


(defun spot4e-retrieve-url-to-alist-synchronously (url)
  "Return alist representation of json response from URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (json-read-from-string
     (decode-coding-region (+ 1 url-http-end-of-headers)
                           (point-max) 'utf-8 t))))


(defun spot4e-get-track-search-results-alist (type q)
  "Return json results from TYPE search for Q (query)."
  (let ((url-request-method "GET")
	(q-params (concat "?q=" q
			  "&type=" type
			  "&limit=" "50"
			  "&access_token=" spot4e-access-token)))
    (spot4e-retrieve-url-to-alist-synchronously (concat spot4e-search-url q-params))))


(defun spot4e-get-currently-playing-context ()
  "Return json results from track search with q=Q."
  (let ((url-request-method "GET")
	(q-params (concat  "?access_token=" spot4e-access-token)))
    (setq spot4e-currently-playing-context-alist
	  (spot4e-retrieve-url-to-alist-synchronously (concat spot4e-currently-playing-url
							      q-params)))))


(defun spot4e-set-currently-playing ()
  "Set track, artist, album name for current track and store in spot4e-currently-playing."
  (interactive)
  (setq spot4e-currently-playing
	(spot4e-format-track-for-mini-buffer-display
	 (alist-get 'item (spot4e-get-currently-playing-context)))))


(defun spot4e-message-currently-playing ()
  "Message track, artist, album name for current track and display in minibuffer."
  (interactive)
  (sleep-for spot4e-wait-time)
  (spot4e-set-currently-playing)
  (message spot4e-currently-playing))


(defun spot4e-player-do-action (method action)
  "Via the METHOD spoecified, send ACTION to player endpoint."
  (interactive)
  (let ((url-request-method method)
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " spot4e-access-token)))))
    (url-retrieve-synchronously (concat spot4e-player-url action)))
  (spot4e-message-currently-playing))


(defun spot4e-player-play ()
  "Press play on Spotify active device."
  (interactive)
  (spot4e-player-do-action "PUT" "/play"))


(defun spot4e-player-pause ()
  "Press pause on Spotify active device."
  (interactive)
  (spot4e-player-do-action "PUT" "/pause"))


(defun spot4e-player-next ()
  "Press next on Spotify active device."
  (interactive)
  (spot4e-player-do-action "POST" "/next"))


(defun spot4e-player-previous ()
  "Press previous on Spotify active device."
  (interactive)
  (spot4e-player-do-action "POST" "/previous"))


(defun spot4e-play-track (track)
  "Play TRACK in context of the album the TRACK appears on."
  (let ((url-request-method "PUT")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat "Bearer " spot4e-access-token))))
	(url-request-data
	 (format "{\"context_uri\":\"%s\", \"offset\":{\"uri\":\"%s\"}}"
		 (alist-get 'uri (alist-get 'album track)) ;album uri
		 (alist-get 'uri track)))) ;track uri
    (url-retrieve-synchronously spot4e-player-play-url))
  (spot4e-message-currently-playing))


(defun spot4e-format-track-for-helm-buffer-display (track)
  "Formats TRACK for display in helm buffer."
  (let ((track-name (alist-get 'name track))
	(artist-name (alist-get 'name (elt (alist-get 'artists track) 0)))
	(album-name (alist-get 'name (alist-get 'album track))))
    (concat track-name "\n"
	    artist-name "  |||  " album-name)))


(defun spot4e-format-track-for-mini-buffer-display (track)
  "Formats TRACK for display in mini buffer."
  (let ((track-name (alist-get 'name track))
	(artist-name (alist-get 'name (elt (alist-get 'artists track) 0)))
	(album-name (alist-get 'name (alist-get 'album track))))
    (concat track-name " --- "
	    artist-name "  |||  " album-name)))


(defun spot4e-tracks-candidates ()
  "Return name of the track (car) with track metadata (cdr)."
  (mapcar
   (lambda (track) (cons (spot4e-format-track-for-helm-buffer-display track) track))
   (alist-get 'items
	      (alist-get 'tracks
			 (spot4e-get-track-search-results-alist "track" helm-pattern)))))



(defun spot4e-helm-search-tracks ()
  "Fucntion to search via helm interface for spotify tracks matching user-input."
  (interactive)
  (helm
   :sources (helm-build-sync-source "spot4e-tracks-candidates"
	      :candidates 'spot4e-tracks-candidates
	      :action '(("Play track" . spot4e-play-track))
	      :volatile t
	      :multiline t)))


(provide 'spot4e)
;;; spot4e.el ends here
