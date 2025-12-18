;;; infrasonic.el --- Subsonic server support for Emacs         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Kai Bagley <kaibagley@proton.mail>
;; Maintainer: Kai Bagley <kaibagley@proton.mail>
;; Keywords: multimedia
;; Package-Requires: ((emacs "30") (plz "0.9"))
;; Version: 0.1.1
;; URL: https://github.com/alphapapa/listen.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;; Requirements

;; TODO: Send bookmark request to server periodically
;; TODO: When emacs 31.1 is released, cl-decf/cl-incf -> decf/incf

(require 'plz)          ; HTTP requests
(require 'auth-source)  ; authinfo
(require 'json)         ; for Emacs < 30 (do i need this?)

(require 'map)          ; for map-nested-elt
(require 'cl-lib)       ; for cl-incf/decf
(require 'url-util)     ; for url-build-query-string

;; Declares

;;;; Customisation

(defgroup infrasonic nil
  "Options for `infrasonic'."
  :group 'multimedia)

(defcustom infrasonic-url ""
  "The fully-qualified domain name of your OpenSubsonic-compatible server.
For example, \"music.example.com\" or \"192.168.0.0:4533\".
Don't include the protocol/scheme or the resource path."
  :type 'string
  :group 'infrasonic)

(defcustom infrasonic-protocol "https"
  "Protocol to use for calls to Subsonic API.
Must be either \"http\" or \"https\" (default)."
  :type '(choice (const :tag "HTTPS" "https")
                 (const :tag "HTTP" "http"))
  :group 'infrasonic)

(defcustom infrasonic-search-max-results 200
  "Maximum results to return in search queries."
  :type 'integer
  :group 'infrasonic)

(defcustom infrasonic-user-agent "infrasonic"
  "User-agent used in API requests.
Used by the server to identify the player."
  :type 'string
  :group 'infrasonic)

(defvar infrasonic--auth-params nil
  "The authentication URL parameters.
This should not be set globally.
For batch operations, this is let-bound.
For other operations, generate on the fly using `infrasonic--get-auth-params'.")

;;;; General helpers

(defun infrasonic--standardise (item type)
  "Standardise ITEM of TYPE.
Returns the standardised ITEM

Ensures a \"name\" element exists in ITEM, and add a \"subsonic-type\"
element according to the ITEMs TYPE."
  (let ((name (or (alist-get 'name item)
                  (alist-get 'title item)
                  (alist-get 'artist item)
                  (alist-get 'album item))))
    (setf (alist-get 'subsonic-type item) type)
    (setf (alist-get 'name item) name))
  item)

;;;; Auth helpers

(defun infrasonic--get-credentials ()
  "Fetch user credentials securely using `auth-source'.
Returns an auth-source plist, or nil if not found.

Searches `auth-source' files for an entry with \":host\" matching `infrasonic-url'."
  (car (auth-source-search :host infrasonic-url)))

(defun infrasonic--get-auth-params ()
  "Return authentication info for Subsonic API calls.
Return an alist of strings: ((\"u\" . \"myusername\") (\"t\" . \"<randomstring>\") ...).

Note that the token and salt are leaked locally in the player's process information."
  (or infrasonic--auth-params
      (let* ((creds (or (infrasonic--get-credentials)
                        (error "No auth-source entry found for host %S" infrasonic-url)))
             (user (plist-get creds :user))
             (pass (funcall (plist-get creds :secret)))
             (salt (format "%06x" (random #xffffff)))
             (token (secure-hash 'md5 (concat pass salt))))
        `(("u" . ,user)
          ("t" . ,token)
          ("s" . ,salt)
          ("v" . "1.16.1")
          ("c" . ,infrasonic-user-agent)
          ("f" . "json")))))

(defun infrasonic--build-url (endpoint params)
  "Build an OpenSubsonic REST API URL from ENDPOINT and PARAMS.
Returns a complete URL required to make an API call.

ENDPOINT is the API method name, see `https://www.subsonic.org/pages/api.jsp' for
details.
PARAMS is an alist of query parameters."
  (let* ((param-list (mapcar (lambda (p)
                               (list (car p) (cdr p)))
                             params))
         (param-str (url-build-query-string param-list nil t)))
    (format "%s://%s/rest/%s.view?%s"
            infrasonic-protocol
            infrasonic-url
            endpoint
            param-str)))

;;;; API Helpers

(defun infrasonic--process-api-response ()
  "Parse JSON response from an OpenSubsonic API request.
Returns data contained in `subsonic-response' alist, or signals an error.

Should be called from a buffer containing an API response."
  (goto-char (point-min))
  (when (zerop (buffer-size))
    (error "API response is empty"))
  (let* ((json-data (json-parse-buffer :object-type 'alist
                                       :null-object nil
                                       :false-object nil
                                       :array-type 'list))
         (response (alist-get 'subsonic-response json-data)))
    (unless (string-equal "ok" (alist-get 'status response))
      (error "API response returned error: %s"
             (alist-get 'message (alist-get 'error response))))
    response))

(defun infrasonic-api-call (endpoint &optional params callback body)
  "Make a call to the OpenSubsonic API.
Returns the parsed JSON if CALLBACK is nil.
Returns the curl process object if CALLBACK is non-nil.

ENDPOINT is the API method name, see `https://www.subsonic.org/pages/api.jsp' for
details.
PARAMS is an alist of additional parameters.
If CALLBACK is nil, run synchronously and parse the JSON response.
If CALLBACK is non-nil, run asynchronously and parse the JSON response, then call CALLBACK on the
parsed JSON.

The JSON should usually be processed by `infrasonic--process-api-response'."
  (when (equal infrasonic-url "")
    (error "Please set `infrasonic-url'"))
  (let* ((api-params (append (infrasonic--get-auth-params) params))
         (http-method (if body 'post 'get))
         (api-url (infrasonic--build-url endpoint api-params))
         (api-headers '(("Accept-Encoding" . "gzip")
                        ("Content-Type" . "application/x-www-form-urlencoded"))))
    (plz http-method api-url
      :headers api-headers
      :body body
      :as #'infrasonic--process-api-response
      :then (or callback 'sync)
      :else (lambda (err) (error "Subsonic API request error: %s" err)))))

(defun infrasonic-get-stream-url (track-id)
  "Create a streaming URL for track with TRACK-ID.
Returns a complete URL for MPV or VLC to directly stream from the server."
  (infrasonic--build-url
   "stream"
   (append (infrasonic--get-auth-params)
           `(("id" . ,track-id)))))

;;;; Requests

(defun infrasonic-ping-server ()
  "Ping the server to check connectivity and authentication.
Returns nil, only displaying a success or failure message."
  (interactive)
  (condition-case err
      (progn
        (infrasonic-api-call "ping")
        (message "Successfully pinged OpenSubsonic server!"))
    (error
     (message "Failed to ping server: %s" (error-message-string err)))))

(defun infrasonic--get-items (endpoint rootkey itemkey &optional params)
  "Get data from ENDPOINT, and extract content using ROOTKEY and ITEMKEY.
Returns a list of items.

ENDPOINT is the API method name, see `https://www.subsonic.org/pages/api.jsp' for
details.
ROOTKEY is the top-level JSON key in the API response, ITEMKEY is the
inner key (for example, \"searchResult3\" and \"song\"). Go to the above link for details.
PARAMS are optional API parameters."
  (when-let* ((response (infrasonic-api-call endpoint params))
              (items (map-nested-elt response (list rootkey itemkey))))
    items))

;;;; ID3 Getters
;; These functions get artists, albums and tracks using the ID3 endpoints
;; They also add:
;; - subsonic-type element (:artist, :album, :track),
;; - name element (tracks use title instead of name, so ensure everything has a name

;;; Artists

(defun infrasonic-get-artists ()
  "Get a list of artists.
Returns a flat list of artists, with (subsonic-type . :artist) appended
to each artist.

The \"getArtists\" endpoint returns a list of lists, each sublist being
artists under the alphabetical index:
(((name . \"#\") (artist . (<list of artists>)))
 ((name . \"a\") (artist . (<list of artists>)))
 ((name . \"b\") (artist . (<list of artists>)))
 ...)
This function returns artists completely flattened:
((<artist>) (<artist>) ...)"
  (let ((indexes (infrasonic--get-items "getArtists"
                                        'artists 'index)))
    ;; flatten the indexes -> list of artist-lists
    (mapcan (lambda (index)
              (mapcar (lambda (artist) (infrasonic--standardise artist :artist))
                      (alist-get 'artist index)))
            indexes)))

;;; Albums

(defun infrasonic-get-album-list (type)
  "Return an album list.
Returns a list of albums. Number returned is dictated by
`infrasonic-search-max-results'.

TYPE may be:
- `:random': Random albums.
- `:newest': Newest albums by release date.
- `:frequent': User's most frequently played albums.
- `:recent': Recently added albums.
- `:starred': Starred albums.
- `:byname': Alphabetically sorted by name.
- `:byartist': Alphabetically sorted by artist. "
  )

(defun infrasonic-get-artist (artist-id)
  "Get a list of albums for artist with ARTIST-ID.
Returns a flat list of albums by artist with ARTIST-ID, with (subsonic-type .
:album) appended to each album.

The \"getArtists\" endpoint returns a list of albums."
  (let ((albums (infrasonic--get-items "getArtist"
                                       'artist 'album
                                       `(("id" . ,artist-id)))))
    (mapcar (lambda (album) (infrasonic--standardise album :album))
            albums)))

;;; Tracks under album

(defun infrasonic-get-album (album-id)
  "Get a list of tracks for album with ALBUM-ID.
Returns a flat list of tracks in album with ALBUM-ID, with (subsonic-type
. :track) and (name . <track name>) appended to each track.

The \"getAlbum\" endpoint returns a list of tracks. The tracks do not
contain a name element like albums and artists do, so we add it here."
  (let ((tracks (infrasonic--get-items "getAlbum"
                                       'album 'song
                                       `(("id" . ,album-id)))))
    (mapcar (lambda (track) (infrasonic--standardise track :track))
            tracks)))

;;; Playlists

(defun infrasonic-get-playlists ()
  "Fetch all of the user's playlists from the server.
Returns an alist mapping playlist names to their IDs: ((name . id) ...)."
  (when-let* ((items (infrasonic--get-items "getPlaylists"
                                            'playlists 'playlist)))
    (mapcar (lambda (item)
              (cons (alist-get 'name item)
                    (format "%s" (alist-get 'id item))))
            items)))

(defun infrasonic-get-playlist-tracks (playlist-id)
  "Fetch all tracks in playlist with PLAYLIST-ID.
Returns a list of parsed JSON tracks."
  (let ((tracks (infrasonic--get-items "getPlaylist"
                                       'playlist 'entry
                                       `(("id" . ,playlist-id)))))
    (mapcar (lambda (track) (infrasonic--standardise track :track))
            tracks)))

;;; Star

(defun infrasonic-get-starred-tracks ()
  "Fetch all starred tracks from the server.
Returns a list of parsed JSON tracks."
  (let ((tracks (infrasonic--get-items "getStarred2"
                                       'starred2 'song)))
    (mapcar (lambda (track) (infrasonic--standardise track :track))
            tracks)))

(defun infrasonic-star (item-id star-p &optional callback)
  "Set ITEM-ID's (artist, album or track) star status according to STAR-P.
Returns the parsed API response.

Send a request to the \"star\" or \"unstar\" Subsonic endpoints, star (when STAR-P is non-nil) or
unstar ITEM-ID. CALLBACK is passed to `infrasonic-api-call' and is evaluated on the response data."
  (infrasonic-api-call (if star-p "star" "unstar")
                        `(("id" . ,item-id))
                        callback))

;;; Scrobble

(defun infrasonic-scrobble (track-id status)
  "Scrobble STATUS for the track with TRACK-ID to the Subsonic API.
Returns the parsed API response.

STATUS may be either `:playing' or `:finished'."
  (let* ((submission (pcase status
                       (:playing "false")
                       (:finished "true")
                       (_ (error "Status invalid: %S. Must be either `:playing' or `:finished'" status))))
         (params `(("id" . ,track-id)
                   ("submission" . ,submission))))
    (infrasonic-api-call "scrobble" params #'ignore)))

;;; Random

(defun infrasonic-get-random-tracks (n)
  "Fetch N random tracks from the server.
Returns a N length list of parsed JSON tracks.

Uses OpenSubsonic API's \"getRandomSongs\" with N as the \"size\"."
  (let ((tracks (infrasonic--get-items "getRandomSongs"
                                       'randomSongs 'song
                                       `(("size" . ,(number-to-string n))))))
    (mapcar (lambda (track) (infrasonic--standardise track :track))
            tracks)))

;;; Search

(defun infrasonic-search (query)
  "Search server for QUERY at \"search3\" endpoint for artists, albums and
tracks.
Returns a flat list of items:
(((subsonic-type . :artist) (name . ...) ...)
 ((subsonic-type . :album) (name . ...) ...)
 ((subsonic-type . :track) (name . ...) ...)
 ((subsonic-type . :track) (name . ...) ...))

Each item is a list with an added element `subsonic-type', and tracks
have the `name' element added."
  (let* ((max-results (number-to-string (/ infrasonic-search-max-results 3)))
         (params `(("query" . ,query)
                   ("artistCount" . ,max-results)
                   ("albumCount" . ,max-results)
                   ("songCount" . ,max-results)))
         (response (infrasonic-api-call "search3" params))
         (result (alist-get 'searchResult3 response))
         (artists (alist-get 'artist result))
         (albums (alist-get 'album result))
         (tracks (alist-get 'song result)))
    (append
     (mapcar (lambda (artist) (infrasonic--standardise artist :artist))
             artists)
     (mapcar (lambda (album) (infrasonic--standardise album :album))
             albums)
     (mapcar (lambda (track) (infrasonic--standardise track :track))
             tracks))))

(defun infrasonic-search-tracks (query)
  "Search the server for tracks matching QUERY.
Returns a list of parsed JSON tracks.

Uses the Subsonic API's \"search3\" endpoint with QUERY as the search query."
  (let ((tracks (infrasonic--get-items "search3"
                                       'searchResult3 'song
                                       `(("query" . ,query)
                                         ("songCount" . ,(number-to-string infrasonic-search-max-results))))))
    (mapcar (lambda (track) (infrasonic--standardise track :track))
            tracks)))

;;; Bulk get tracks

(defun infrasonic-get-all-tracks (item-id level)
  "Fetch all tracks under item (artist or album) associated with ITEM-ID.
Returns a list of parsed JSON tracks.

LEVEL determines what level of the hierarchy we are on:
- :artist: fetches all albums, then all tracks by that artist.
- :album: fetches all tracks on the album."
  (let ((infrasonic--auth-params (infrasonic--get-auth-params)))
    (pcase level
      (:artist
       (let ((albums (infrasonic--get-items "getArtist"
                                            'artist 'album
                                            `(("id" . ,item-id)))))
         (mapcan (lambda (album)
                   (infrasonic-get-all-tracks (alist-get 'id album) :album))
                 albums)))
      (:album
       (let ((tracks (infrasonic--get-items "getAlbum"
                                            'album 'song
                                            `(("id" . ,item-id)))))
         (mapcar (lambda (track) (infrasonic--standardise track :track))
                 tracks))))))

;;;; Write requests

(defun infrasonic-create-playlist (track-ids name)
  "Create a playlist with NAME containing TRACK-IDS on the server.
Returns the newly created playlist.

TRACK-IDS should be a list of strings.
NAME should be a string.

This function uses a POST request since large playlists can return HTTP error 414."
  ;; we have to pass one songId per track
  (let* ((body-list (cons `("name" ,name)
                          (mapcar (lambda (id)
                                    (list "songId" id))
                                  track-ids)))
         (body-str (url-build-query-string body-list nil t))
         (playlist (infrasonic-api-call "createPlaylist"
                                        nil nil
                                        body-str)))
    (if playlist
        (progn
          (message "Playlist '%s' was created with '%d' tracks."
                   name (length track-ids))
          playlist)
      (error "Playlist was not created."))))

(defun infrasonic-delete-playlist (playlist-id)
  "Delete a Subsonic playlist with PLAYLIST-ID."
  (when (infrasonic-api-call "deletePlaylist"
                              `(("id" . ,playlist-id)))
    (message "Playlist deleted.")))

;;; Art

(defun infrasonic-get-art-url (item-id &optional size)
  "Return the art URL for ITEM-ID.

SIZE is the size of the cover art."
  (infrasonic--build-url "getCoverArt"
                         (append (infrasonic--get-auth-params)
                                 `(("id" . ,item-id)
                                   ("size" . ,(if size (number-to-string size) "64"))))))

(defun infrasonic-get-art (url file &optional callback)
  "Download cover art at URL and write to FILE.
Returns the path to the downloaded file.

CALLBACK is called when the art is downloaded. This may be used to
display the image in FILE somewhere in Emacs once it's finished
downloading."
  (plz 'get url
    :as `(file ,file)
    :then (or callback 'sync)
    :else (lambda (err)
            (error "Subsonic download failed: %s" err))))

;;; High-level stuff

(defun infrasonic-children (id level)
  "Fetch \"nodes\" for directory hierarchy LEVEL and ID.
Returns a list of alists, each alist representing children of ID.

This is intended as a helper function for accessing the OpenSubsonic
server as a directory structure.

LEVEL determines the endpoint to use, and may be one of:
- :artists: Returns top-level view of all artists using endpoint \"getArtists\".
- :artist: Returns albums for an artist using \"getArtist\".
- :album: Returns songs in an album using \"getAlbum\"."
  (let ((items
         (pcase level
           (:artists (infrasonic-get-artists))
           (:artist (infrasonic-get-artist id))
           (:album (infrasonic-get-album id)))))
    items))

(defun infrasonic-child (level)
  "Determines the hierarchical level under LEVEL.
Returns the keyword symbol for the next level.

Hierarchy is: :artists -> :artist -> :album."
  (pcase level
    (:artists :artist)
    (:artist :album)
    (:album :track)
    (_ :track)))


(provide 'infrasonic)

;;; infrasonic.el ends here
