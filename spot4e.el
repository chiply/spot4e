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
		     "user-library-modify "
		     "user-library-read "
		     "user-follow-read "
		     "user-read-recently-played")
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
(defvar spot4e-browse-url "https://api.spotify.com/v1/browse")
(defvar spot4e-playlist-url "https://api.spotify.com/v1/users/spotify/playlists")
(defvar spot4e-new-releases-url "https://api.spotify.com/v1/browse/new-releases")
(defvar spot4e-albums-url "https://api.spotify.com/v1/albums")
(defvar spot4e-artist-url "https://api.spotify.com/v1/artists")
(defvar spot4e-recommendations-url "https://api.spotify.com/v1/recommendations")
(defvar spot4e-me-url "https://api.spotify.com/v1/me")
(defvar spot4e-users-url "https://api.spotify.com/v1/users")
(defvar spot4e-following-url "https://api.spotify.com/v1/me/following")
(defvar spot4e-tracks-url "https://api.spotify.com/v1/tracks/")
(setq spot4e-recently-played-url "https://api.spotify.com/v1/me/player/recently-played")


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
  ;; Copy auth URL to kill ring (to respect terminal emacs users)
  (kill-new spot4e-auth-url-full)
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


(defun spot4e-helm-formatter (item track-address artist-address context-address)
  "Generic function to format requested data for display.
ITEM is the alist containing the data.  TRACK-ADDRESS, ARTIST-ADDRESS
and CONTEXT-ADDRESS are the alist-get-chain arguments which point
to the track, artist, and context, respctively"
  (let ((track-name (when track-address
		      (alist-get-chain track-address item)))
	(artist-name (when artist-address
		       (alist-get-chain
			(list (car (last artist-address)))
			(elt (alist-get-chain
			      (butlast artist-address) item)
			     0))))
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
HELM-SOURCE-NAME will be the name of the helm-source.  URL is the
url to the spotify api enpoint DYNAMIC-HELM specifies whether the
api request should be updated with helm-pattern Q-PARAMS are the
query paremters to be appended to the URL ALIST-ADDRESS will be
the chain address of the candidate residing within the alist
response TRACK-ADDRESS is the chain address of the track within
the candidate alist ARTIST-ADDRESS is the chain address of the
PLAYLIST within the candidate alist CONTEXT-ADDRESS is the chain
address of the CONTEXT within the candidate alist HELM-ACTIONS is
an alist representing the actions on a candidate."
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


(defun spot4e-format-track-for-mini-buffer-display (item)
  "Formats ITEM for display in mini buffer."
  (let ((track-name (alist-get-chain '(name) item))
	(artist-name (alist-get-chain '(name) (elt (alist-get-chain '(artists) item) 0)))
	(album-name (alist-get-chain '(album name) item)))
    (concat track-name " --- "
	    artist-name "  |||  " album-name)))


(defun spot4e-helm-tracks (type extra-q-params &optional selection goback-alist)
  "Displays list of tracks in helm-buffer for interaction.
TYPE indicates the type of tracks, i.e. playlists, albums, search
etc... You can pass EXTRA-Q-PARAMS to the query if necessary.
This is relevant because each type has a different set of
addresses for getting the data out.  SELECTION is an alist
selection (from precvious helm buffer) from which to extract
alist, name, artist, and context via ALIST-ADDRESS, NAME-ADDRESS,
ARTIST-ADDRESS, CONTEXT-ADDRESS."
  (let ((url (cond ((equal type "search")
		    spot4e-search-url)
		   ((equal type "album")
		    (concat spot4e-albums-url
			    "/"
			    (alist-get-chain '(id) selection)))
		   ((equal type "user")
		    (concat spot4e-me-url "/tracks"))
		   ((equal type "rec")
		    spot4e-recommendations-url)
		   ((equal type "playlist")
		    (progn
		      (setq spot4e-playlist-uri
			    (alist-get-chain '(uri) selection))
		      (concat "https://api.spotify.com/v1/users"
			      "/"
			      (alist-get-chain '(owner id) selection)
			      "/"
			      "playlists"
			      "/"
			      (alist-get-chain '(id) selection))))
		   ((equal type "recent")
		    spot4e-recently-played-url)))
	(alist-address (cond ((or (equal type "search") (equal type "album") (equal type "playlist"))
			      '(tracks items))
			     ((or (equal type "user") (equal type "recent"))
			      '(items))
			     ((equal type "rec")
			      '(tracks))))
	(name-address (cond ((or (equal type "search") (equal type "rec") (equal type "album"))
			     '(name))
			    ((or (equal type "user") (equal type "playlist") (equal type "recent"))
			     '(track name))))
	(artist-address (cond ((or (equal type "search") (equal type "rec") (equal type "album"))
			       '(artists name))
			      ((or (equal type "user") (equal type "playlist") (equal type "recent"))
			       '(track artists name))))
	(context-address (cond ((or (equal type "search") (equal type "rec"))
				'(album name))
			       ((equal type "album")
				nil)
			       ((or (equal type "user") (equal type "playlist") (equal type "recent"))
				'(track album name)))))
    (spot4e-helm "spot4e-track-candidates"
		 url
		 (when (equal type "search")
		   t)
		 (concat (concat "?access_token=" spot4e-access-token
				 "&limit=" "50")
			 extra-q-params)
		 alist-address name-address artist-address context-address
		 '(("Play Track" . (lambda (candidate)
				     (spot4e-play-track type candidate)))
		   ("Go Back" . (lambda (candidate)
				  (spot4e-goback-from-tracks-fn goback-alist)))
		   ("Get recommendations" . (lambda (candidate)
					      (spot4e-helm-search-recommendation-tracks
					       type
					       candidate)))
		   ("Save track" . (lambda (candidate)
				     (spot4e-save type candidate)))))))


(defun spot4e-helm-albums (type extra-q-params &optional selection)
  "Displays list of albums in helm-buffer for interaction.
TYPE indicates the type of albums, i.e. new-releases, artist, search
etc... You can pass EXTRA-Q-PARAMS to the query if necessary.
This is relevant because each type has a different set of
addresses for getting the data out.  SELECTION is an alist
selection (from precvious helm buffer) from which to extract
data via ALIST-ADDRESS."
  (let ((url (cond ((equal type "search")
		    spot4e-search-url)
		   ((equal type "artist")
		    (concat spot4e-artist-url
			    "/"
			    (alist-get-chain '(id) selection)
			    "/"
			    "albums"))
		   ((equal type "new")
		    spot4e-new-releases-url)))
	(alist-address (if (or (equal type "search")
			       (equal type "new"))
			   '(albums items)
			 '(items))))
    (spot4e-helm "spot4e-album-candidates"
		 url
		 (when (equal type "search")
		   t)
		 (concat (concat "?access_token=" spot4e-access-token
				 "&limit=" "50")
			 extra-q-params)
		 alist-address nil '(artists name) '(name)
		 '(("Display Album Tracks" . (lambda (candidate) (spot4e-helm-search-album-tracks
							     candidate
							     selection)))
		   ("Go Back" . (lambda (candidate)
				  (spot4e-goback-from-albums-fn)))))))


(defun spot4e-helm-artists (type extra-q-params)
    "Displays list of artists in helm-buffer for interaction.
TYPE indicates the type of artists, i.e. user, search
etc... You can pass EXTRA-Q-PARAMS to the query if necessary.
This is relevant because each type has a different set of
addresses for getting the data out.  SELECTION is an alist
selection (from precvious helm buffer) from which to extract
data."
  (let ((url (cond ((equal type "search")
		    spot4e-search-url)
		   ((equal type "user")
		    spot4e-following-url))))
    (spot4e-helm "spot4e-artist-candidates"
		 url
		 (when (equal type "search")
		   t)
		 (concat (concat "?access_token=" spot4e-access-token
				 "&limit=" "50")
			 extra-q-params)
		 '(artists items) nil nil '(name)
		 '(("Display Album Tracks" . (lambda (candidate)
					       (spot4e-helm-search-artist-albums
						candidate)))))))


(defun spot4e-helm-playlists (type &optional selection)
    "Displays list of playlists in helm-buffer for interaction.
TYPE indicates the type of playlists, i.e. featured, user,
category etc...  This is relevant because each type has a
different set of addresses for getting the data out.  SELECTION
is an alist selection (from precvious helm buffer) from which to
extract data via ALIST-ADDRESS."
  (interactive)
  (let ((url (cond ((equal type "cat")
		    (concat spot4e-categories-url
			    "/"
			    (alist-get-chain '(id) selection)
			    "/"
			    "playlists"))
		   ((equal type "feat")
		    (concat spot4e-browse-url
			    "/"
			    "featured-playlists"))
		   ((equal type "user")
		    (concat spot4e-me-url
			    "/"
			    "playlists"))))
	(alist-address (if (or (equal type "cat")
			       (equal type "feat"))
			   '(playlists items)
			 '(items))))
    (spot4e-helm "spot4e-cetegory-playlist-candidates"
		 url
		 nil
		 (concat "?access_token=" spot4e-access-token
			 "&limit=" "50")
		 alist-address nil nil '(name)
		 '(("Display Playlist Tracks" . (lambda (candidate)
						  (spot4e-helm-search-playlist-tracks
						   candidate
						   selection)))
		   ("Go Back" . (lambda (candidate)
				  (spot4e-helm-search-categories)))))))


(defun spot4e-helm-search-categories ()
  "Display list of spotify categories in helm buffer for interaction."
  (interactive)
  (spot4e-helm "spot4e-categories-candidates"
	       spot4e-categories-url
	       nil
	       (concat  "?access_token=" spot4e-access-token)
	       '(categories items) nil nil '(name)
	       '(("Display Category Playlists" . (lambda (candidate)
						   (spot4e-helm-search-category-playlists candidate))))))


(defun spot4e-helm-search-artists ()
  "Displays list of artists in helm buffer for interaction given helm-pattern."
  (interactive)
  (fset 'spot4e-goback-from-albums-fn 'spot4e-helm-search-artists)
  (spot4e-helm-artists "search" (concat "&type=" "artist")))


(defun spot4e-helm-search-user-artists ()
  "Displays list of user-artists in helm buffer for interaction."
  (interactive)
  (fset 'spot4e-goback-from-albums-fn 'spot4e-helm-search-user-artists)
  (spot4e-helm-artists "user" (concat "&type=" "artist")))


(defun spot4e-helm-search-albums (&optional goback-alist)
  "Displays list of albums in helm buffer for interaction given helm-pattern.
This interface can be returned to with a goback action in the
subsequent helm-buffer, GOBACK-ALIST is the 'selection' from that
buffer and is ignored."
  (interactive)
  (fset 'spot4e-goback-from-tracks-fn 'spot4e-helm-search-albums)
  (spot4e-helm-albums "search" (concat "&type=" "album")))


(defun spot4e-helm-search-artist-albums (selection)
  "Displays list of artists-albums in helm buffer for interaction given SELECTION."
  (interactive)
  (fset 'spot4e-goback-from-tracks-fn 'spot4e-helm-search-artist-albums)
  (spot4e-helm-albums "artist" (concat "&album_type=" "album,single") selection))


(defun spot4e-helm-search-new-releases (&optional goback-alist)
  "Display list of spotify new album releases in helm buffer for interaction.
GOBACK-ALIST is the helm selection and is not used."
  (interactive)
  (fset 'spot4e-goback-from-tracks-fn 'spot4e-helm-search-new-releases)
  (spot4e-helm-albums "new" nil))


(defun spot4e-helm-search-category-playlists (selection)
  "Displays list of cateogry-playlists in helm buffer for interaction given SELECTION."
  (interactive)
  (fset 'spot4e-goback-from-tracks-fn 'spot4e-helm-search-category-playlists)
  (spot4e-helm-playlists "cat" selection))


(defun spot4e-helm-search-featured-playlists (&optional goback-alist)
  "Displays list of featured-playlists in helm buffer for interaction.
This interface can be returned to with a goback action in the
subsequent helm-buffer, GOBACK-ALIST is the 'selection' from that
buffer and is ignored."
  (interactive)
  (fset 'spot4e-goback-from-tracks-fn 'spot4e-helm-search-featured-playlists)
  (spot4e-helm-playlists "feat"))


(defun spot4e-helm-search-user-playlists (&optional goback-alist)
  "Displays list of user-playlists in helm buffer for interaction.
This interface can be returned to with a goback action in the
subsequent helm-buffer, GOBACK-ALIST is the 'selection' from that
buffer and is ignored."
  (interactive)
  (fset 'spot4e-goback-from-tracks-fn 'spot4e-helm-search-user-playlists)
  (spot4e-helm-playlists "user"))


(defun spot4e-helm-search-album-tracks (selection &optional goback-alist)
  "Displays list of album-tracks in helm buffer for interaction given SELECTION.
This interface can be returned to with a goback action in the
subsequent helm-buffer, GOBACK-ALIST is the 'selection' from that
buffer and is ignored."
  (interactive)
  (spot4e-helm-tracks "album" nil selection goback-alist))


(defun spot4e-helm-search-tracks ()
  "Displays list of user-artists in helm buffer for interaction given helm-pattern."
  (interactive)
  (spot4e-helm-tracks "search" (concat "&type=" "track")))


(defun spot4e-helm-search-playlist-tracks (selection &optional goback-alist)
  "Displays list of playlists-tracks in helm buffer for interaction given SELECTION.
This interface can be returned to with a goback action in the
subsequent helm-buffer, GOBACK-ALIST is the 'selection' from that
buffer and is ignored."
  (interactive)
  (spot4e-helm-tracks "playlist" nil selection goback-alist))
  

(defun spot4e-helm-search-user-tracks ()
  "Displays list of user tracks in helm buffer for interaction."
  (interactive)
  (spot4e-helm-tracks "user" nil))


(defun spot4e-helm-search-recent-tracks ()
  "Displays list of recently played tracks in helm buffer for interaction."
  (interactive)
  (spot4e-helm-tracks "recent" nil))


(defun spot4e-helm-search-recommendation-tracks (&optional type selection)
  "Get recommendations based upon currently playing track or track selected (SELECTION) from any helm-tracks interface.
type of track object is given by TYPE."
  (interactive)
  (let ((track-id (if selection
		      (alist-get-chain
		       (if (or (equal type "search")
			       (equal type "album")
			       (equal type "rec"))
			   '(id)
			 '(track id))
		       selection)
		    (alist-get-chain '(item id) (spot4e-get-currently-playing-context)))))
    (message track-id)
    (spot4e-helm-tracks "rec" (concat "&seed_tracks=" track-id
				      "&access_token=" spot4e-access-token
				      "&limit=" "50"))))


(defun spot4e-play-track (type track)
  "Play track, given by SELECTION, in context of the track
appears on.  TYPE of track object given by TYPE."
  (let ((alist (spot4e-request "GET"
			       (concat spot4e-tracks-url
				       (alist-get-chain
					(if (or (equal type "search")
						(equal type "album")
						(equal type "rec"))
					    '(id)
					  '(track id))
					track))
			       (concat "?access_token="
				       spot4e-access-token)
			       t)))
    (spot4e-request "PUT"
		    spot4e-player-play-url
		    (concat "?access_token=" spot4e-access-token)
		    nil
		    nil
		    (format "{\"context_uri\":\"%s\", \"offset\":{\"uri\":\"%s\"}}"
			    (if (equal type "playlist")
				spot4e-playlist-uri
			      (alist-get-chain '(album uri) alist))
			    (alist-get-chain '(uri) alist))))
  (spot4e-message-currently-playing))


(defun spot4e-save (&optional type selection)
  "Save currently playing track, or track represented by
SELECTION.  Type of track object given by TYPE"
  (interactive)
  (let ((track-id (if selection
		      (alist-get-chain
		       (if (or (equal type "search")
			       (equal type "album")
			       (equal type "rec"))
			   '(id)
			 '(track id))
		       selection)
		    (alist-get-chain '(item id) (spot4e-get-currently-playing-context)))))
    (spot4e-request "PUT"
		    (concat spot4e-me-url "/tracks")
		    (concat "?access_token=" spot4e-access-token
			    "&ids=" track-id))))


(provide 'spot4e)
;;; spot4e.el ends here
