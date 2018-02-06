;;; spot4e.el --- Control Spotify playback with Helm.
;; Copyright 2018 Charlie Baker
;;
;; Author: Charlie Baker <mister.chiply@gmail.com>
;; Maintainer: Charlie Baker <mister.chiply@gmail.
;; URL: *** 
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
       "&scope=" "streaming%20user-read-birthdate%20user-read-email%20user-read-private"
       "&show_dialog=" "true"))
(defvar spot4e-token-url "https://accounts.spotify.com/api/token")
(defvar spot4e-search-url "https://api.spotify.com/v1/search")
(defvar spot4e-player-play-url "https://api.spotify.com/v1/me/player/play")


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
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (+ 1 url-http-end-of-headers))
    (json-read-object)))

(defun spot4e-search-json-results (type q)
  "Return json results from track search with q=Q."
  (let ((url-request-method "GET")
	(q-params (concat "?q=" q
			  "&type=" type
			  "&limit=" "50"
			  "&access_token=" spot4e-access-token)))
    (spot4e-retrieve-url-to-alist-synchronously (concat spot4e-search-url q-params))))


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
  (message "Enjoy the music ;-)"))

(defun spot4e-format-track-for-display (track)
  "Formats TRACK for display in helm buffer."
  (let ((track-name (alist-get 'name track))
	(artist-name (alist-get 'name (elt (alist-get 'artists track) 0)))
	(album-name (alist-get 'name (alist-get 'album track))))
    (concat track-name "\n"
	    artist-name "  |||  " album-name)))

;;;###autoload
(defun spot4e-tracks-candidates ()
  "Return name of the track (car) with track metadata (cdr)."
  (mapcar
   (lambda (track) (cons (spot4e-format-track-for-display track) track))
   (alist-get 'items
	      (alist-get 'tracks
			 (spot4e-search-json-results "track" helm-pattern)))))

;;;###autoload
(defun spot4e-search-tracks ()
  "Fucntion to search via helm interface for spotify tracks matching SEARCH-QUERY."
  (interactive)
  (helm
   :sources (helm-build-sync-source "spot4e-tracks-candidates"
	      :candidates 'spot4e-tracks-candidates
	      :action '(("Play track" . spot4e-play-track))
	      :volatile t
	      :multiline t)))

(provide 'spot4e)
;;; spot4e.el ends here
