;;; infrasonic.el --- Subsonic server support for Emacs         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Kai Bagley <kaibagley@proton.mail>
;; Maintainer: Kai Bagley <kaibagley@proton.mail>
;; Keywords: multimedia
;; Package-Requires: ((emacs "30") (plz "0.9"))
;; Version: 0.1.1
;; URL: https://github.com/kaibagley/infrasonic

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


;; Infrasonic is a small client library for the OpenSubsonic REST API that
;; provides:
;;
;; - Token/salt authentication using `auth-source'.
;; - HTTP requests using `plz'.
;; - JSON parsing.
;; - Response standardisation: items are tagged with:
;;   (subsonic-type . :artist/:album/:track), and
;;   (name . "...")
;;
;; Configuration:
;;
;; Set `infrasonic-url', `infrasonic-protocol' and `infrasonic-user-agent'.
;; Credentials in an `auth-source' backend (e.g. ~/.authinfo) with \"machine\"
;; matching `infrasonic-url'.
;;
;; Data:
;;
;; `infrasonic' functions return parsed OpenSubsonic objects as alists, or
;; lists of alists.
;; Other than the above changes, OpenSubsonic responses are returned basically
;; as-is.
;;
;; Errors:
;;
;; Functions signal `infrasonic-error' for local errors and
;; `infrasonic-api-error' for API failures.

;;; Code:

;;;; Requirements

;; TODO: Send bookmark request to server periodically
;; TODO: When emacs 31.1 is released, cl-decf/cl-incf -> decf/incf

(require 'plz)          ; HTTP requests
(require 'auth-source)  ; authinfo
(require 'subr-x)       ; for when-let
(require 'gv)           ; for setf

(require 'map)          ; for map-nested-elt
(require 'url-util)     ; for url-build-query-string

;; Declares

;;;; Customisation

(defvar infrasonic-url ""
  "The fully-qualified domain name of your OpenSubsonic-compatible server.
For example, \"music.example.com\" or \"192.168.0.0:4533\".
Don't include the protocol/scheme or the resource path.")

(defvar infrasonic-protocol "https"
  "Protocol to use for calls to Subsonic API.
Must be either \"http\" or \"https\" (default).")

(defvar infrasonic-search-max-results 200
  "Maximum results to return in search queries.")

(defvar infrasonic-user-agent "infrasonic"
  "User-agent used in API requests.
Used by the server to identify the player.")

(defvar infrasonic-art-size 64
  "Size of the square cover art images.")

(defvar infrasonic--auth-params nil
  "The authentication URL parameters.
This should not be set globally.
For batch operations, this is let-bound.
For other operations, generate on the fly using `infrasonic--get-auth-params'.")

(defconst infrasonic-opensubsonic-version "1.16.1"
  "OpenSubsonic API version of the server.")

(define-error 'infrasonic-error "Infrasonic error")
(define-error 'infrasonic-api-error "Infrasonic API error" 'infrasonic-error)

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

(defun infrasonic--standardise-list (items type)
  "Standardise all ITEMS as TYPE.
Returns a list of standardised ITEMs."
  (mapcar (lambda (item) (infrasonic--standardise item type))
          items))

(defun infrasonic--alist-p (l)
  "Returns t if L is an alist."
  (and (listp l)                ; its a list
       (consp l)                ; its a cons
       (consp (car l))          ; its an alist
       (not (consp (caar l))))) ; its not a list of alists

(defun infrasonic--ensure-alist-list (l)
  "Return L as a list of alists."
  (cond ((null l) nil)                 ; l empty, return nil
        ((infrasonic--alist-p l) (list l)) ; l single cons, wrap list
        (t l)))

(defun infrasonic--get-one (endpoint key &optional params type)
  "Get data from ENDPOINT that returns one item, and extract contents in KEY.
Returns an alist.

PARAMS are optional API parameters.
TYPE is the type of data (artist, album, track)."
  (when-let* ((response (infrasonic-api-call endpoint params))
              (item (alist-get key response)))
    (if type
        (infrasonic--standardise item type)
      item)))

(defun infrasonic--get-many (endpoint keys &optional params type)
  "Get data from ENDPOINT, and extract content using KEYS.
Returns a list of alists.

KEYS is a list of the outer and inner keys of the OpenSubsonic API
response (for example, \"searchResult3\" and \"song\"). PARAMS are
optional API parameters. TYPE is the type of the items being retrieved
\(:artist, :album, etc.)"
  (when-let* ((response (infrasonic-api-call endpoint params))
              (items (map-nested-elt response keys))
              (items-alist (infrasonic--ensure-alist-list items)))
    (if type
        (infrasonic--standardise-list items-alist type)
      items-alist)))

;;;; Auth helpers

(defun infrasonic--get-credentials ()
  "Fetch user credentials securely using `auth-source'.
Returns an auth-source plist, or nil if not found.

Searches `auth-source' files for an entry with \":host\" matching
`infrasonic-url'."
  (car (auth-source-search :host infrasonic-url
                           :require '(:user :secret)
                           :max 1)))

(defun infrasonic--get-auth-params ()
  "Return authentication info for Subsonic API calls.
Return an alist of strings:
\((\"u\" . \"myusername\") (\"t\" . \"<randomstring>\") ...).

Note that the token and salt are leaked locally in the player's process
information."
  (or infrasonic--auth-params
      (let* ((creds (or (infrasonic--get-credentials)
                        (signal 'infrasonic-error
                                (list "No auth-source entry found for host"
                                      infrasonic-url))))
             (user (plist-get creds :user))
             (secret (plist-get creds :secret))
             (pass (if (functionp secret) (funcall secret) secret))
             (salt (format "%06x" (random #xffffff)))
             (token (secure-hash 'md5 (concat pass salt))))
        `(("u" . ,user)
          ("t" . ,token)
          ("s" . ,salt)
          ("v" . ,infrasonic-opensubsonic-version)
          ("c" . ,infrasonic-user-agent)
          ("f" . "json")))))

(defun infrasonic--build-url (endpoint params)
  "Build an OpenSubsonic REST API URL from ENDPOINT and PARAMS.
Returns a complete URL required to make an API call.

ENDPOINT is the API method name, see `https://www.subsonic.org/pages/api.jsp'
for details.
PARAMS is an alist of query parameters."
  (let* ((param-list (mapcar (lambda (p)
                               (list (car p) (cdr p)))
                             params))
         (param-str (url-build-query-string param-list nil t)))
    (concat
     (format "%s://%s/rest/%s.view"
             infrasonic-protocol
             infrasonic-url
             endpoint)
     (when (and param-str (not (string-empty-p param-str)))
       (format "?%s" param-str)))))

;;;; API Helpers

(defun infrasonic--process-api-response ()
  "Parse JSON response from an OpenSubsonic API request.
Returns data contained in `subsonic-response' alist, or signals an error.

Should be called from a buffer containing an API response."
  (goto-char (point-min))
  (when (zerop (buffer-size))
    (signal 'infrasonic-api-error (list "API response is empty")))
  (condition-case err
      (let* ((json-data (json-parse-buffer :object-type 'alist
                                           :null-object nil
                                           :false-object nil
                                           :array-type 'list))
             (response (alist-get 'subsonic-response json-data))
             (status (alist-get 'status response)))
        (unless response
          (signal 'infrasonic-api-error (list "Bad or missing response data" json-data)))
        (unless (and (stringp status) (string-equal "ok" status))
          (let* ((err (alist-get 'error response))
                 (msg (alist-get 'message err))
                 (cod (alist-get 'code err)))
            (signal 'infrasonic-api-error (list msg cod err))))
        response)
    (json-parse-error
     (signal 'infrasonic-error (list "Error parsing JSON response"
                                     (error-message-string err))))))

(defun infrasonic-api-call (endpoint &optional params callback body)
  "Make a call to the OpenSubsonic API.
Returns the parsed JSON if CALLBACK is nil.
Returns the curl process object if CALLBACK is non-nil.

ENDPOINT is the API method name, see `https://www.subsonic.org/pages/api.jsp'
for details.
PARAMS is an alist of additional parameters.
If CALLBACK is nil, run synchronously and parse the JSON response.
If CALLBACK is non-nil, run asynchronously and parse the JSON response, then
call CALLBACK on the parsed JSON.

The JSON should usually be processed by `infrasonic--process-api-response'."
  (when (equal infrasonic-url "")
    (signal 'infrasonic-error (list "Please set `infrasonic-url'")))
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
      :else (lambda (err)
              (if callback
                  (message "Subsonic API request error: %s" err)
                (signal 'infrasonic-api-error (list "Subsonic API request error"
                                                    err)))))))

(defun infrasonic-get-stream-url (track-id)
  "Create a streaming URL for track with TRACK-ID.
Returns a complete URL for MPV or VLC to directly stream from the server."
  (infrasonic--build-url
   "stream"
   (append (infrasonic--get-auth-params)
           `(("id" . ,track-id)))))

;;;;; Endpoints

;;;; System

;;; ping

(defun infrasonic-ping-server ()
  "Ping the server to check connectivity and authentication.
Returns nil, only displaying a success or failure message."
  (interactive)
  (condition-case err
      (progn
        (infrasonic-api-call "ping")
        (message "Successfully pinged OpenSubsonic server!"))
    (infrasonic-error
     (message "Failed to ping server: %s" (error-message-string err)))))

;;; getLicense

(defun infrasonic-get-license ()
  "Get the license status from the OpenSubsonic server.
Returns the parsed license response."
  (alist-get 'license (infrasonic-api-call "getLicense")))

;;;; Browsing
;; Not including: getMusicFolders, getIndexes, getMusicDirectory, getVideos,
;; getVideoInfo, getArtistInfo, getAlbumInfo, getSimilarSongs

;;; getGenres

;;; getArtists

(defun infrasonic-get-artists ()
  "Get a list of artists.
Returns a flat list of artists, with (subsonic-type . :artist) appended
to each artist.

The \"getArtists\" endpoint returns a list of lists, each sublist being
artists under the alphabetical index:
\(((name . \"#\") (artist . (<list of artists>)))
 ((name . \"a\") (artist . (<list of artists>)))
 ((name . \"b\") (artist . (<list of artists>)))
 ...)
This function returns artists completely flattened:
\((<artist>) (<artist>) ...)"
  ;; Dont tag with :artist yet, since these are indices
  (let ((indexes (infrasonic--get-many "getArtists" '(artists index))))
    ;; flatten the indexes -> list of artist-lists
    (mapcan (lambda (index)
              (let ((artists (infrasonic--ensure-alist-list (alist-get 'artist index))))
                (infrasonic--standardise-list artists :artist)))
            indexes)))

;;; getArtist

(defun infrasonic-get-artist (artist-id)
  "Get a list of albums for artist with ARTIST-ID.
Returns a flat list of albums by artist with ARTIST-ID, with (subsonic-type .
:album) appended to each album.

The \"getArtists\" endpoint returns a list of albums."
  (infrasonic--get-many "getArtist"
                        '(artist album)
                        `(("id" . ,artist-id))
                        :album))

;;; getAlbum

(defun infrasonic-get-album (album-id)
  "Get a list of tracks for album with ALBUM-ID.
Returns a flat list of tracks in album with ALBUM-ID, with (subsonic-type
. :track) and (name . <track name>) appended to each track.

The \"getAlbum\" endpoint returns a list of tracks. The tracks do not
contain a name element like albums and artists do, so we add it here."
  (infrasonic--get-many "getAlbum"
                        '(album song)
                        `(("id" . ,album-id))
                        :track))

;;; getSong

(defun infrasonic-get-song (track-id)
  "Get information for a track with TRACK-ID.
Returns the parsed list of track attributes."
  (infrasonic--get-one "getSong"
                       'song
                       `(("id" . ,track-id))
                       :track))

;;; getArtistInfo2

(defun infrasonic-get-artist-info (artist-id)
  "Get information about the artist with ARTIST-ID.
Returns the parsed list of artist attributes."
  (let ((artist-info (infrasonic-api-call "getArtistInfo2"
                                          `(("id" . ,artist-id)))))
    (alist-get 'artistInfo2 artist-info)))

;;; getAlbumInfo2

(defun infrasonic-get-album-info (album-id)
  "Get information about the album with ALBUM-ID.
Returns the parsed list of album attributes."
  (let ((album-info (infrasonic-api-call "getAlbumInfo2"
                                         `(("id" . ,album-id)))))
    ;; For some reason getAlbumInfo2 has nested albumInfo element (no 2)
    (alist-get 'albumInfo album-info)))

;;; getSimilarSongs2

(defun infrasonic-get-similar-songs (artist-id &optional n)
  "Get N random songs from ARTIST-ID and from similar artists.
Returns a list of songs."
  (let ((n-tracks (or n infrasonic-search-max-results)))
    (infrasonic--get-many "getSimilarSongs2"
                          '(similarSongs2 song)
                          `(("id" . ,artist-id)
                            ("count" . ,(number-to-string n-tracks)))
                          :track)))

;;; getTopSongs

(defun infrasonic-get-top-songs (artist-id &optional n)
  "Get ARTIST-ID's N top songs.
Returns a list of songs."
  (let ((n-tracks (or n infrasonic-search-max-results)))
    (infrasonic--get-many "getTopSongs"
                          '(topSongs song)
                          `(("id" . ,artist-id)
                            ("count" . ,(number-to-string n-tracks)))
                          :track)))

;;;; Album/song lists
;; Not including: getAlbumList, getStarred

;;; getAlbumList2

(defun infrasonic-get-album-list (type &optional n)
  "Return a list of N albums according to TYPE.
Returns a list of albums. Number returned is dictated by either N, or
`infrasonic-search-max-results' by default.

TYPE may be:
- A genre string, for example: \"Rock\".
- `:random': Random albums.
- `:newest': Newest albums by release date.
- `:frequent': User's most frequently played albums.
- `:recent': Recently added albums.
- `:starred': Starred albums.
- `:byname': Alphabetically sorted by name.
- `:byartist': Alphabetically sorted by artist."
  (let* ((n-albums (or n infrasonic-search-max-results))
         (list-type (pcase type
                      (:random "random")
                      (:newest "newest")
                      (:frequent "frequent")
                      (:recent "recent")
                      (:starred "starred")
                      (:byname "alphabeticalByName")
                      (:byartist "alphabeticalByArtist")
                      (_ "byGenre")))
         (params (append
                  `(("type" . ,list-type)
                    ("size" . ,(number-to-string n-albums)))
                  (when (stringp type)
                    `(("genre" . ,type))))))
    (infrasonic--get-many "getAlbumList2"
                          '(albumList2 album)
                          params
                          :album)))

;;; getRandomSongs

(defun infrasonic-get-random-songs (&optional n)
  "Fetch N random tracks from the server.
Returns a N length list of parsed JSON tracks.

Uses OpenSubsonic API's \"getRandomSongs\" with N as the \"size\"."
  (let ((n-tracks (or n infrasonic-search-max-results)))
    (infrasonic--get-many "getRandomSongs"
                          '(randomSongs song)
                          `(("size" . ,(number-to-string n-tracks)))
                          :track)))

;;; getSongsByGenre

(defun infrasonic-get-songs-by-genre (genre &optional n)
  "Get N songs in GENRE.
Returns a list of songs."
  (let ((n-tracks (or n infrasonic-search-max-results)))
    (infrasonic--get-many "getSongsByGenre"
                          '(songsByGenre song)
                          `(("genre" . ,genre)
                            ("count" . ,(number-to-string n-tracks)))
                          :track)))

;;; Playlists

(defun infrasonic-get-playlists ()
  "Fetch all of the user's playlists from the server.
Returns an alist mapping playlist names to their IDs: ((name . id) ...)."
  (when-let* ((items (infrasonic--get-many "getPlaylists"
                                           '(playlists playlist)
                                           nil
                                           :playlist)))
    (mapcar (lambda (item)
              (cons (alist-get 'name item)
                    (format "%s" (alist-get 'id item))))
            items)))

(defun infrasonic-get-playlist-tracks (playlist-id)
  "Fetch all tracks in playlist with PLAYLIST-ID.
Returns a list of parsed JSON tracks."
  (infrasonic--get-many "getPlaylist"
                        '(playlist entry)
                        `(("id" . ,playlist-id))
                        :track))

;;; Star

(defun infrasonic-get-starred-tracks ()
  "Fetch all starred tracks from the server.
Returns a list of parsed JSON tracks."
  (infrasonic--get-many "getStarred2" '(starred2 song) nil :track))

;;;; Setters

;;; Star

(defun infrasonic-star (item-id star-p &optional callback)
  "Set ITEM-ID's (artist, album or track) star status according to STAR-P.
Returns the parsed API response.

Send a request to the \"star\" or \"unstar\" Subsonic endpoints, star (when
STAR-P is non-nil) or unstar ITEM-ID.
CALLBACK is passed to `infrasonic-api-call' and is evaluated on the response
data."
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
                       (_ (signal 'infrasonic-error (list "Status invalid. Must be either `:playing' or `:finished'"
                                                          status)))))
         (params `(("id" . ,track-id)
                   ("submission" . ,submission))))
    (infrasonic-api-call "scrobble" params #'ignore)))

;;; Search

(defun infrasonic-search (query)
  "Search for QUERY at \"search3\" endpoint for artists, albums and tracks.
Returns a flat list of items:
\(((subsonic-type . :artist) (name . ...) ...)
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
         (artists (infrasonic--ensure-alist-list (alist-get 'artist result)))
         (albums (infrasonic--ensure-alist-list (alist-get 'album result)))
         (tracks (infrasonic--ensure-alist-list (alist-get 'song result))))
    (append
     (infrasonic--standardise-list artists :artist)
     (infrasonic--standardise-list albums :album)
     (infrasonic--standardise-list tracks :track))))

(defun infrasonic-search-tracks (query n)
  "Search the server for N tracks matching QUERY.
Returns a list of parsed JSON tracks.

Uses the Subsonic API's \"search3\" endpoint with QUERY as the search query."
  (let ((n-tracks (or n infrasonic-search-max-results)))
    (infrasonic--get-many "search3"
                          '(searchResult3 song)
                          `(("query" . ,query)
                            ("songCount" . ,(number-to-string n-tracks)))
                          :track)))

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
       (mapcan (lambda (album)
                 (infrasonic-get-all-tracks (alist-get 'id album) :album))
               (infrasonic-get-artist item-id)))
      (:album
       (infrasonic-get-album item-id)))))

;;;; Write requests

(defun infrasonic-create-playlist (track-ids name)
  "Create a playlist with NAME containing TRACK-IDS on the server.
Returns the newly created playlist.

TRACK-IDS should be a list of strings.
NAME should be a string.

This function uses a POST request since large playlists can return HTTP error
414."
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
      (signal 'infrasonic-error (list "Playlist was not created.")))))

(defun infrasonic-delete-playlist (playlist-id)
  "Delete a Subsonic playlist with PLAYLIST-ID."
  (when (infrasonic-api-call "deletePlaylist"
                              `(("id" . ,playlist-id)))
    (message "Playlist deleted.")))

;;; Art

(defun infrasonic-get-art-url (item-id &optional size)
  "Return the art URL for ITEM-ID.

SIZE is the size of the cover art."
  (let ((art-size (or size infrasonic-art-size)))
    (infrasonic--build-url "getCoverArt"
                           (append (infrasonic--get-auth-params)
                                   `(("id" . ,item-id)
                                     ("size" . ,(number-to-string art-size)))))))

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
            (signal 'infrasonic-api-error (list "Subsonic download failed"
                                                err)))))

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
