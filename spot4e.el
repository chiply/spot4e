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
(defvar spot4e-categories-url "https://api.spotify.com/v1/browse/categories")
(defvar spot4e-playlist-url "https://api.spotify.com/v1/users/spotify/playlists")


(defun spot4e-retrieve-url-to-alist-synchronously (url)
  "Return alist representation of json response from URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (json-read-from-string
     (decode-coding-region (+ 1 url-http-end-of-headers)
                           (point-max) 'utf-8 t))))


(defun spot4e-request (spot4e-request-method spot4e-request-url &optional spot4e-request-q-params spot4e-request-parse-json spot4e-request-extra-headers spot4e-request-data)
  "Function to handle spot4e requests."
  (let ((url-request-method spot4e-request-method)
	(url-request-data spot4e-request-data)
	(url-request-extra-headers spot4e-request-extra-headers))
    (if spot4e-request-parse-json
	(spot4e-retrieve-url-to-alist-synchronously
	 (concat spot4e-request-url spot4e-request-q-params))
      (url-retrieve-synchronously
       (concat spot4e-request-url spot4e-request-q-params)))))


(defun spot4e-authorize ()
  "Obtain access_ and refresh_ tokens for user account."
  (interactive)
  (browse-url spot4e-auth-url-full)
  (setq spot4e-auth-code (read-string "Enter code from URL: "))
  (setq spot4e-tokens-alist
	(spot4e-request "POST"
			spot4e-token-url
			(concat "?grant_type=" "authorization_code"
				"&redirect_uri=" spot4e-redirect-uri
				"&code=" spot4e-auth-code)
			t
			`(("Content-Type" . "application/x-www-form-urlencoded")
			  ("Authorization" . ,(concat "Basic " spot4e-b64-id-secret)))
			nil
			t))
    (setq spot4e-access-token
	  (alist-get 'access_token spot4e-tokens-alist))
    (setq spot4e-refresh-token
	  (alist-get 'refresh_token spot4e-tokens-alist)))


(defun spot4e-refresh ()
  "Obtain access_ and refresh_ tokens for user account."
  (interactive)
  (setq spot4e-refresh-alist
	
	(spot4e-request "POST"
			spot4e-token-url
			(concat "?grant_type=" "refresh_token"
				"&refresh_token=" spot4e-refresh-token)
			t
			`(("Content-Type" . "application/x-www-form-urlencoded")
			  ("Authorization" . ,(concat "Basic " spot4e-b64-id-secret)))
			nil))
    (setq spot4e-access-token
	  (alist-get 'access_token spot4e-refresh-alist)))


(defun spot4e-get-track-search-results-alist (type q)
  "Return json results from TYPE search for Q (query)."
  (spot4e-request "GET"
		  spot4e-search-url
		  (concat "?q=" q
			  "&type=" type
			  "&limit=" "50"
			  "&access_token=" spot4e-access-token)
		  t))


(defun spot4e-get-currently-playing-context ()
  "Return json results from track search with q=Q."
  (setq spot4e-currently-playing-context-alist
	(spot4e-request "GET"
			spot4e-currently-playing-url
			(concat  "?access_token=" spot4e-access-token)
			t)))

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
  
  (spot4e-request method
		  (concat spot4e-player-url action)
		  (concat  "?access_token=" spot4e-access-token))
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
  (spot4e-request "PUT"
		  spot4e-player-play-url
		  (concat "?access_token=" spot4e-access-token)
		  nil
		  nil
		  (format "{\"context_uri\":\"%s\", \"offset\":{\"uri\":\"%s\"}}"
			  (alist-get 'uri (alist-get 'album track))
			  (alist-get 'uri track)))
  (spot4e-message-currently-playing))


(defun spot4e-helm (spot4e-helm-source-name spot4e-helm-source-candidates spot4e-helm-actions)
  (helm
   :sources (helm-build-sync-source spot4e-helm-source-name
	      :candidates spot4e-helm-source-candidates
	      :action spot4e-helm-actions
	      :volatile t
	      :multiline t)))


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
  (spot4e-helm "spot4e-tracks-candidates" 'spot4e-tracks-candidates
	       '(("Play track" . spot4e-play-track))))


(defun spot4e-get-categories-alist ()
  "Get list of spotofy categories."
  (spot4e-request "GET"
		  spot4e-categories-url
		  (concat  "?access_token=" spot4e-access-token)
		  t))


(defun spot4e-categories-candidates ()
  "Return name of the category (car) with category metadata (cdr)."
  (mapcar
   (lambda (category) (cons (alist-get 'name category) category))
   (alist-get 'items
	      (alist-get 'categories
			 (spot4e-get-categories-alist)))))


(defun spot4e-helm-search-categories ()
  "Display list of spotify categories in helm buffer for interaction."
  (interactive)
  (spot4e-helm "spot4e-categories-candidates" 'spot4e-categories-candidates
	       '(("Display Category Playlists" . spot4e-helm-search-category-playlists))))


(defun spot4e-get-category-playlists-alist (spot4e-category-id)
  "Get alist of playlists for a give SPOT4E-CATEGORY-ID."
  (spot4e-request "GET"
		  (concat spot4e-categories-url
			  "/"
			  spot4e-category-id
			  "/playlists")
		  (concat "?access_token=" spot4e-access-token
			  "&limit=" "50")
		  t))


(defun spot4e-category-playlist-candidates (spot4e-category-id)
  "Return name of playlist (car) with playlist metadata (cdr) for a given SPOT4E-CATEGORY-ID."
  (mapcar
   (lambda (playlist) (cons (alist-get 'name playlist) playlist))
   (alist-get 'items
	      (alist-get 'playlists
			 (spot4e-get-category-playlists-alist spot4e-category-id)))))


(defun spot4e-helm-search-category-playlists (spot4e-category-alist)
  "Display list of playlists for given SPOT4E-CATEGORY-ALIST in helm buffer for interaction."
  (interactive)
  (let ((spot4e-category-id (alist-get 'id spot4e-category-alist)))
    (spot4e-helm "spot4e-category-playlists" (spot4e-category-playlist-candidates spot4e-category-id)
		 '(("Display Playlist Tracks" . spot4e-helm-search-playlist-tracks)))))


(defun spot4e-get-playlist-tracks-alist (spot4e-playlist-id)
  "Return alist of tracks for a given SPOT4E-PLAYLIST-ID."
  (spot4e-request "GET"
		  (concat spot4e-playlist-url "/" spot4e-playlist-id)
		  (concat "?access_token=" spot4e-access-token)
		  t))


(defun spot4e-format-playlist-tracks-for-helm-buffer-display (playlist-track-alist)
  "Format playlist's tracks in PLAYLIST-TRACK-ALIST for display in helm buffer."
  (let ((track-name (alist-get 'name (alist-get 'track playlist-track-alist)))
	(artist-name (alist-get 'name (elt (alist-get 'artists (alist-get 'track playlist-track-alist)) 0)))
	(album-name (alist-get 'name (alist-get 'album (alist-get 'track playlist-track-alist)))))
    (concat track-name "\n"
	    artist-name "  |||  " album-name)))


(defun spot4e-playlist-tracks-candidates (spot4e-playlist-id)
  "Return playlist track name (car) with playlist track metadata (cdr) for a given SPOT4E-PLAYLIST-ID."
  (mapcar
   (lambda (playlist) (cons (spot4e-format-playlist-tracks-for-helm-buffer-display playlist) playlist))
   (alist-get 'items
	      (alist-get 'tracks
			 (spot4e-get-playlist-tracks-alist spot4e-playlist-id)))))


(defun spot4e-play-playlist-track (playlist-track-alist)
  "Play track in context of the playlist the PLAYLIST-TRACK-ALIST appears on."
  (spot4e-request "PUT"
		  spot4e-player-play-url
		  (concat "?access_token=" spot4e-access-token)
		  nil
		  nil
		  (format "{\"context_uri\":\"%s\", \"offset\":{\"uri\":\"%s\"}}"
			  spot4e-playlist-uri
			  (alist-get 'uri (alist-get 'track playlist-track-alist))))
  (spot4e-message-currently-playing))


(defun spot4e-helm-search-playlist-tracks (spot4e-playlist-alist)
  "Display list of tracks in a playlist, given by SPOT4E-PLAYLIST-ALIST, in helm buffer for interaction."
  (interactive)
  (let ((spot4e-playlist-id (alist-get 'id spot4e-playlist-alist))
	(spot4e-user-id (alist-get 'id (alist-get 'owner spot4e-playlist-alist))))
    (setq spot4e-playlist-uri (concat "spotify:user:"
				      spot4e-user-id
				      ":playlist:"
				      spot4e-playlist-id))
    (spot4e-helm "spot4e-playlist-tracks" (spot4e-playlist-tracks-candidates spot4e-playlist-id)
		 '(("Play track" . spot4e-play-playlist-track)))))


(provide 'spot4e)
;;; spot4e.el ends here
