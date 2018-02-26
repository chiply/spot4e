;;; spot4e.el --- Control Spotify playback with Helm.
;; Copyright 2018 Charlie Baker
;;
;; Author: Charlie Baker <mister.chiply@gmail.com>
;; Maintainer: Charlie Baker <mister.chiply@gmail.com>
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
		     "user-read-playback-state "
		     "user-library-modify")
   "&show_dialog=" "true"))
(defvar spot4e-token-url "https://accounts.spotify.com/api/token")
(defvar spot4e-search-url "https://api.spotify.com/v1/search")

;; there are synchronicity issues when displaying currently-playing
;; after doing player action (i.e. next, previous), even when using
;; spot4e-message-currently-playing as a callback funtion, so for now
;; force sleep time between changing track and calling
;; spot4e-message-currently-playing to resolve this issue.
(defvar spot4e-wait-time 1.0)
(defvar spot4e-player-url "https://api.spotify.com/v1/me/player")
(defvar spot4e-player-play-url (concat spot4e-player-url "/play"))
(defvar spot4e-currently-playing-url (concat spot4e-player-url "/currently-playing"))
(defvar spot4e-categories-url "https://api.spotify.com/v1/browse/categories")
(defvar spot4e-playlist-url "https://api.spotify.com/v1/users/spotify/playlists")
(defvar spot4e-new-releases-url "https://api.spotify.com/v1/browse/new-releases")
(defvar spot4e-albums-url "https://api.spotify.com/v1/albums")
(defvar spot4e-recommendations-url "https://api.spotify.com/v1/recommendations")
(defvar spot4e-me-url "https://api.spotify.com/v1/me")

(fset 'alist-get-chain 'alist-get)
(defun alist-get-chain (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get-chain (cdr symbols)
		       (assoc (car symbols) alist))
    (cdr alist)))


(defun spot4e-retrieve-url-to-alist-synchronously (url)
  "Return alist representation of json response from URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (json-read-from-string
     (decode-coding-region (+ 1 url-http-end-of-headers)
                           (point-max) 'utf-8 t))))


(defun spot4e-request (method url &optional q-params parse-json extra-headers data)
  "Function to handle spot4e requests.
METHOD is the request method, URL is the URL, Q-PARAMS is the
query parameters, PARSE-JSON is a boolean for whether to parse
and return the json response as an alist, EXTRA-HEADERS is an
alist of headers, and DATA is request body data as JSON."
  (let ((url-request-method method)
	(url-request-data data)
	(url-request-extra-headers extra-headers))
    (if parse-json
	(spot4e-retrieve-url-to-alist-synchronously
	 (concat url q-params))
      (url-retrieve-synchronously
       (concat url q-params)))))


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
			nil))
  (setq spot4e-access-token
	(alist-get-chain '(access_token) spot4e-tokens-alist))
  (setq spot4e-refresh-token
	(alist-get-chain '(refresh_token) spot4e-tokens-alist)))


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
	(alist-get-chain '(access_token) spot4e-refresh-alist)))


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
	 (alist-get-chain '(item) (spot4e-get-currently-playing-context)))))


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
			  (alist-get-chain '(album uri) track)
			  (alist-get-chain '(uri) track)))
  (spot4e-message-currently-playing))

(defun spot4e-helm-formatter (item track-address artist-address context-address)
  "Generic function to format requested data for display.  ITEM
is the alist containing the data.  TRACK-ADDRESS, ARTIST-ADDRESS
and CONTEXT-ADDRESS are the alist-get-chain arguments which point
to the track, artist, and context, respctively"
  (let ((track-name (when track-address
		      (alist-get-chain track-address item)))
	(artist-name (when artist-address
		       (alist-get-chain
			(list (car (last artist-address))) (elt (alist-get-chain (butlast artist-address) item) 0))))
	(context-name (when context-address
			(alist-get-chain context-address item))))
    (cond ((and track-name artist-name context-name)
	   (concat track-name "\n"
		   artist-name "  |||  " context-name))
	  ((and artist-name context-name (eq track-name nil))
	   (concat context-name " ||| " artist-name))
	  ((and context-name (eq track-name nil) (eq artist-name nil))
	   context-name)
	  ((and track-name artist-name (eq context-name nil)
		(concat track-name " ||| "artist-name))))))


(defun spot4e-helm-candidates ()
  "Return name of the candidate (car) with candidate metadata (cdr)."
  (mapcar (lambda (candidate) (cons
			       (spot4e-helm-formatter candidate
						      spot4e-track-address
						      spot4e-artist-address
						      spot4e-context-address)
			       candidate))
	  (alist-get-chain spot4e-alist-address
			   (if (eq spot4e-dynamic-helm t)
			       (spot4e-request "GET" spot4e-url
					       (concat spot4e-q-params "&q=" helm-pattern) t)
			     (spot4e-request "GET" spot4e-url spot4e-q-params t)))))


;;  include candidates fn in the spot4e-helm function?
(defun spot4e-helm (helm-source-name
		    url
		    dynamic-helm
		    q-params
		    alist-address
		    track-address
		    artist-address
		    context-address
		    helm-actions)
  "Create helm buffer given args.
HELM-SOURCE-NAME will be the name of the helm-source.
URL is the url to the spotify api enpoint
DYNAMIC-HELM specifies whether the api request should be updated with helm-pattern
Q-PARAMS are the query paremters to be appended to the URL
ALIST-ADDRESS will be the chain address of the candidate residing within the alist response
TRACK-ADDRESS is the chain address of the track within the candidate alist
ARTIST-ADDRESS is the chain address of the PLAYLIST within the candidate alist
CONTEXT-ADDRESS is the chain address of the CONTEXT within the candidate alist
HELM-ACTIONS is an alist representing the actions on a candidate."
  (setq spot4e-url url)
  (setq spot4e-dynamic-helm dynamic-helm)
  (setq spot4e-q-params q-params)
  (setq spot4e-alist-address alist-address)
  (setq spot4e-track-address track-address)
  (setq spot4e-artist-address artist-address)
  (setq spot4e-context-address context-address)
  (helm
   :sources (helm-build-sync-source helm-source-name
	      :candidates 'spot4e-helm-candidates
	      :action helm-actions
	      :volatile t
	      :multiline t)))


(defun spot4e-format-track-for-mini-buffer-display (track)
  "Formats TRACK for display in mini buffer."
  (let ((track-name (alist-get-chain '(name) track))
	(artist-name (alist-get-chain '(name) (elt (alist-get-chain '(artists) track) 0)))
	(album-name (alist-get-chain '(album name) track)))
    (concat track-name " --- "
	    artist-name "  |||  " album-name)))


(defun spot4e-helm-search-tracks ()
  "Fucntion to search via helm interface for spotify tracks matching user-input."
  (interactive)
  (spot4e-helm "spot4e-tracks-candidates"
	       spot4e-search-url
	       t
	       (concat "?type=" "track"
		       "&limit=" "50"
		       "&access_token=" spot4e-access-token)
	       '(tracks items) '(name) '(artists name) '(album name)
	       '(("Play track" . spot4e-play-track)
		 ("Get recommendations" . spot4e-helm-search-recommendations-track)
		 ("Save track" . spot4e-save))))


;; browsing function
(defun spot4e-helm-search-categories (&optional spot4e-goback)
  "Display list of spotify categories in helm buffer for interaction.
SPOT4E-GOBACK is the helm selection and is not used."
  (interactive)
  (spot4e-helm "spot4e-categories-candidates"
	       spot4e-categories-url
	       nil
	       (concat  "?access_token=" spot4e-access-token)
	       '(categories items) nil nil '(name)
	       '(("Display Category Playlists" . spot4e-helm-search-category-playlists))))


(defun spot4e-helm-search-category-playlists (spot4e-category-alist)
  "Display list of playlists for given SPOT4E-CATEGORY-ALIST in helm buffer for interaction."
  (interactive)
  (setq spot4e-category-alist-goback spot4e-category-alist)
  (let ((spot4e-category-id (alist-get-chain '(id) spot4e-category-alist)))
    (spot4e-helm "spot4e-cetegory-playlist-candidates"
		 (concat spot4e-categories-url
			 "/"
			 spot4e-category-id
			 "/playlists")
		 nil
		 (concat "?access_token=" spot4e-access-token
			 "&limit=" "50")
		 '(playlists items) nil nil '(name)
		 '(("Display Playlist Tracks" . spot4e-helm-search-playlist-tracks)
		   ("Go Back" . spot4e-helm-search-categories)))))


(defun spot4e-play-playlist-track (playlist-track-alist)
  "Play track in context of the playlist the PLAYLIST-TRACK-ALIST appears on."
  (spot4e-request "PUT"
		  spot4e-player-play-url
		  (concat "?access_token=" spot4e-access-token)
		  nil
		  nil
		  (format "{\"context_uri\":\"%s\", \"offset\":{\"uri\":\"%s\"}}"
			  spot4e-playlist-uri
			  (alist-get-chain '(track uri) playlist-track-alist)))
  (spot4e-message-currently-playing))


(defun spot4e-helm-goback-to-playlists (&optional spot4e-goback)
  "Go back to playlists from trakcs.  SPOT4E-GOBACK is the helm selection and is not used."
  (spot4e-helm-search-category-playlists spot4e-category-alist-goback))


(defun spot4e-helm-search-playlist-tracks (spot4e-playlist-alist)
  "Display list of tracks in a playlist, given by SPOT4E-PLAYLIST-ALIST, in helm buffer for interaction."
  (interactive)
  (let ((spot4e-playlist-id (alist-get-chain '(id) spot4e-playlist-alist))
	(spot4e-user-id (alist-get-chain '(owner id) spot4e-playlist-alist)))
    (setq spot4e-playlist-uri (concat "spotify:user:"
				      spot4e-user-id
				      ":playlist:"
				      spot4e-playlist-id))
    (spot4e-helm "spot4e-playlist-tracks"
		 (concat spot4e-playlist-url "/" spot4e-playlist-id)
		 nil
		 (concat "?access_token=" spot4e-access-token)
		 '(tracks items) '(track name) '(track artists name) '(track album 'name)
		 '(("Play track" . spot4e-play-playlist-track)
		   ("Go Back" . spot4e-helm-goback-to-playlists)))))


(defun spot4e-helm-search-new-releases (&optional spot4e-goback)
  "Display list of spotify new album releases in helm buffer for interaction.
SPOT4E-GOBACK is the helm selection and is not used."
  (interactive)
  (spot4e-helm "spot4e-new-releases-candidates"
	       spot4e-new-releases-url
	       nil
	       (concat  "?access_token=" spot4e-access-token
			"&limit=" "50")
	       '(albums items) nil '(artists name) '(name)
	       '(("Search Album Tracks" . spot4e-helm-search-album-tracks))))


(defun spot4e-play-album-track (spot4e-album-track-alist)
  "Play track in context of the album the album (SPOT4E-ALBUM-TRACK-ALIST) appears on."
  (spot4e-request "PUT"
		  spot4e-player-play-url
		  (concat "?access_token=" spot4e-access-token)
		  nil
		  nil
		  (format "{\"context_uri\":\"%s\", \"offset\":{\"uri\":\"%s\"}}"
			  spot4e-album-uri
			  (alist-get-chain '(uri) spot4e-album-track-alist)))
  (spot4e-message-currently-playing))


(defun spot4e-helm-search-album-tracks (spot4e-album-alist)
  "Display list of spotify album's (SPOT4E-ALBUM-ALIST) tracks in helm buffer for interaction."
  (let ((spot4e-album-id (alist-get-chain '(id) spot4e-album-alist)))
    (setq spot4e-album-uri (concat "spotify:album:"
				   spot4e-album-id))
    (spot4e-helm "spot4e-album-tracks-candidates"
		 (concat spot4e-albums-url
			 "/"
			 spot4e-album-id)
		 nil
		 (concat "?access_token=" spot4e-access-token
			 "&limit=" "50")
		 '(tracks items) '(name) '(artists name) nil
		 '(("Play track" . spot4e-play-album-track)
		   ("Go Back" . spot4e-helm-search-new-releases)))))


(defun spot4e-helm-search-recommendations-track (&optional track-alist)
  "Get recommendations based upon currently playing track or track selected (TRACK-ALIST) in spot4e-helm-search-tracks."
  (interactive)
  ;(spot4e-set-currently-playing)
  (let ((track-id (if track-alist
		      (alist-get-chain '(id) track-alist)
		    (alist-get-chain '(item id) (spot4e-get-currently-playing-context)))))
    (spot4e-helm "spot4e-recommendations"
		 spot4e-recommendations-url
		 nil
		 (concat "?seed_tracks=" track-id
			 "&access_token=" spot4e-access-token)
		 '(tracks) '(name) '(artists name) '(album name)
		 '(("Play Track" . spot4e-play-track)))))


;;user data functions
(defun spot4e-save (&optional track-alist)
  "Save currently playing track, or track represented by TRACK-ALIST."
  (interactive)
  (let ((track-id (if track-alist
		      (alist-get-chain '(id) track-alist)
		    (alist-get-chain '(item id) (spot4e-get-currently-playing-context)))))
    (spot4e-request "PUT"
		    (concat spot4e-me-url "/tracks")
		    (concat "?access_token=" spot4e-access-token
			    "&ids=" track-id))))


(provide 'spot4e)
;;; spot4e.el ends here
