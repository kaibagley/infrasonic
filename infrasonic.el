;;; infrasonic.el --- Subsonic server support for Emacs         -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Kai Bagley <kaibagley+github@proton.mail>
;; Maintainer: Kai Bagley <kaibagley+github@proton.mail>
;; Keywords: multimedia
;; Package-Requires: ((emacs "30") (plz "0.9"))
;; Version: 0.2.0
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
;;   (subsonic-type . :artist/:album/:song/:playlist/:genre), and
;;   (name . "...")
;;
;; Configuration:
;;
;; Create a client with `infrasonic-make-client', and pass it to API functions.
;; Credentials come from `auth-source'. :host should be the client's :url
;; (subsonic host).
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

;; TODO: changelog file
;; TODO: contributing guide

;;; Code:

;;;; Requirements

(require 'plz)          ; HTTP requests
(require 'auth-source)  ; authinfo
(require 'json)
(require 'cl-lib)

(require 'subr-x)       ; for when-let
(require 'gv)           ; for setf
(require 'map)          ; for map-nested-elt
(require 'url-util)     ; for url-build-query-string

;; Declares

;;;; Customisation

;; Errors

(define-error 'infrasonic-error "Infrasonic error")
(define-error 'infrasonic-api-error "Infrasonic API error" 'infrasonic-error)
(define-error 'infrasonic-filesystem-error "Infrasonic filesystem error" 'infrasonic-error)

;;;; General helpers

;;; Client

(cl-defstruct (infrasonic-client (:constructor infrasonic--make-client)
                                 (:copier nil))
  "Struct used for storing information about the `infrasonic' client."
  (url
   nil
   :type (or string null)
   :documentation "URL of the OpenSubsonic client, not including the protocol. E.g.
\"music.example.com\". No default.")
  (protocol
   "https"
   :type string
   :documentation "Protocol of the OpenSubsonic client. Defaults to \"https\".")
  (user-agent
   "infrasonic"
   :type string
   :documentation "User agent to report to the OpenSubsonic server. Defaults to
\"infrasonic\".")
  (api-version
   "1.16.1"
   :type string
   :documentation "Version of the OpenSubsonic API to use. Defaults to \"1.16.1\".")
  (queue-limit
   5
   :type natnum
   :documentation "Max items allowed on the download queue. Defaults to 5.")
  (timeout
   300
   :type natnum
   :documentation "Timeout in seconds for any given API request. Defaults to 300.")
  (art-size
   64
   :type natnum
   :documentation "Default resolution in pixels of images to request for art downloads.
Defaults to 64.")
  (search-max-results
   200
   :type natnum
   :documentation "Default maximum results to return in any given API request. Defaults to
200.")
  (cached-credentials
   nil
   :documentation "Cached auth-source credentials. Not set by the user."))

(defun infrasonic-make-client (&rest plist)
  "Return an `infrasonic' client PLIST.

Should be comprised of (default values):
- :url                (no default)
- :protocol           (\"https\")
- :user-agent         (\"infrasonic\")
- :api-version        (\"1.16.1\")
- :queue-limit        (5)
- :timeout            (300)
- :art-size           (64)
- :search-max-results (200)"
  (let ((client (apply #'infrasonic--make-client plist)))
    (unless (and (stringp (infrasonic-client-url client))
                 (not (string-empty-p (infrasonic-client-url client))))
      (signal 'infrasonic-error '("Client missing valid :url")))
    (unless (member (infrasonic-client-protocol client) '("http" "https"))
      (signal 'infrasonic-error
              (list "Client has invalid :protocol" (infrasonic-client-protocol client))))
    client))

(defun infrasonic--standardise (item type)
  "Standardise ITEM of TYPE.
Returns the standardised ITEM. Also standardises ITEM in-place.

Ensures a \"name\" element exists in ITEM, and add a \"subsonic-type\"
element according to the ITEMs TYPE."
  (let ((name (or (alist-get 'name item)
                  (alist-get 'title item)
                  (alist-get 'artist item)
                  (alist-get 'album item))))
    (nconc item (list (cons 'subsonic-type type)))
    (if (alist-get 'name item)
        (setf (alist-get 'name item) name) ; in place update
      (nconc item (list (cons 'name name)))))
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
  (cond ((null l) nil)                     ; l empty, return nil
        ((infrasonic--alist-p l) (list l)) ; l single cons, wrap list
        (t l)))

(defun infrasonic--safe-filename (filename)
  "Return a safened FILENAME string.

Removes newlines, symbols, etc. replacing them with \"_\"."
  (replace-regexp-in-string "[/\\:*?\"<>|\n\r\t]" "_" filename))

(defun infrasonic-child (level)
  "Determines the hierarchical level under LEVEL.
Returns the keyword symbol for the next level.

Hierarchy is:
:artists -> :artist -> :album    -> song
                       :playlist -> song
                       :genre    -> song"
  (pcase level
    (:artists :artist)
    (:artist :album)
    ((or :album :playlist :genre) :song)
    (_ :song)))

(defun infrasonic--get-one (client endpoint key &optional params type)
  "Get one item from ENDPOINT using CLIENT and extract contents in KEY.
Returns an alist.

PARAMS are optional API parameters.
TYPE is the type of data (artist, album, song)."
  (when-let* ((response (infrasonic-api-call client endpoint params))
              (item (alist-get key response)))
    (if type
        (infrasonic--standardise item type)
      item)))

(defun infrasonic--get-many (client endpoint keys &optional params type)
  "Get many items from ENDPOINT using CLIENT and extract content using KEYS.
Returns a list of alists.

KEYS is a list of the outer and inner keys of the OpenSubsonic API
response (for example, \"searchResult3\" and \"song\"). PARAMS are
optional API parameters. TYPE is the type of the items being retrieved
\(:artist, :album, etc.)"
  (when-let* ((response (infrasonic-api-call client endpoint params))
              (items (map-nested-elt response keys))
              (items-alist (infrasonic--ensure-alist-list items)))
    (if type
        (infrasonic--standardise-list items-alist type)
      items-alist)))

;;;; Auth helpers

(defun infrasonic--get-credentials (client)
  "Fetch CLIENT's user credentials securely using `auth-source'.
Returns an auth-source plist, or nil if not found.

Searches `auth-source' files for an entry with \":host\" matching client :url."
  (car (auth-source-search :host (infrasonic-client-url client)
                           :require '(:user :secret)
                           :max 1)))

(defun infrasonic-clear-credentials (client)
  "Clear CLIENT's cached user credentials."
  (setf (infrasonic-client-cached-credentials client) nil))

(defun infrasonic--get-auth-params (client)
  "Return CLIENT's authentication info for Subsonic API calls.
Return an alist of strings:
\((\"u\" . \"myusername\") (\"t\" . \"<randomstring>\") ...).

Note that the token and salt are leaked locally in the player's process
information."
  (let* ((host (infrasonic-client-url client))
         (api-version (infrasonic-client-api-version client))
         (user-agent (infrasonic-client-user-agent client))
         ;; Check cred cache first, if miss, then set cache
         (creds (or (infrasonic-client-cached-credentials client)
                    (let ((fnd (infrasonic--get-credentials client)))
                      (when fnd
                        (setf (infrasonic-client-cached-credentials client) fnd))
                      fnd)
                    (signal 'infrasonic-error
                            (list "No auth-source entry found for host" host))))
         (user (plist-get creds :user))
         (secret (plist-get creds :secret))
         (pass (if (functionp secret) (funcall secret) secret))
         (salt (format "%06x" (random #xffffff)))
         (token (secure-hash 'md5 (concat pass salt))))
    `(("u" . ,user)
      ("t" . ,token)
      ("s" . ,salt)
      ("v" . ,api-version)
      ("c" . ,user-agent)
      ("f" . "json"))))

(defun infrasonic--build-url (client endpoint params)
  "Build an OpenSubsonic REST API URL from ENDPOINT and PARAMS using CLIENT.
Returns a complete URL required to make an API call.

ENDPOINT is the API method name, see `https://www.subsonic.org/pages/api.jsp'
for details.
PARAMS is an alist of query parameters."
  (let* ((host (infrasonic-client-url client))
         (protocol (infrasonic-client-protocol client))
         (param-list (mapcar (lambda (p)
                               (list (car p) (cdr p)))
                             params))
         (param-str (url-build-query-string param-list nil t)))
    (concat
     (format "%s://%s/rest/%s.view"
             protocol
             host
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

(defun infrasonic--request (queue &rest args)
  "Submit a HTTP request, or add it to QUEUE.

If QUEUE is non-nil, add a `plz' request with ARGS to a `plz-queue', and
return QUEUE.
If QUEUE is nil, call `plz' immediately with ARGS and return the `plz' request
object."
  (if queue
      (apply #'plz-queue queue args)
    (apply #'plz args)))

(defun infrasonic-api-call (client endpoint &optional params callback errback body queue)
  "Make a call to the OpenSubsonic API from CLIENT.
If QUEUE is nil, return parsed JSON (sync) or the `plz' process object (async).
If QUEUE is non-nil, return the `plz-queue'.

ENDPOINT is the API method name, see `https://www.subsonic.org/pages/api.jsp'
for details.

PARAMS is an alist of additional parameters.

CALLBACK and ERRBACK are the callback and error callback functions to be
used when making an asynchronous API call. When non-nil, the request
will be made asynchronously automatically.

If BODY is non-nil, use a POST request rather than GET, and BODY is the body of
the POST.

If QUEUE is non-nil, add the request to a `plz-queue'. When QUEUE is non-nil,
CALLBACK must be provided.

The JSON should usually be processed by `infrasonic--process-api-response'."
  (cl-check-type client infrasonic-client)
  (when (and queue (not (functionp callback)))
    (signal 'infrasonic-error (list "Queued API calls must be asynchronous, ensure CALLBACK is a function")))
  (let* ((api-params (append (infrasonic--get-auth-params client) params))
         (api-url (infrasonic--build-url client endpoint api-params))
         (api-headers '(("Accept-Encoding" . "gzip")
                        ("Content-Type" . "application/x-www-form-urlencoded"))))
    (infrasonic--request
     queue
     (if body 'post 'get)
     api-url
     :headers api-headers
     :body body
     :as #'infrasonic--process-api-response
     :then (or callback 'sync)
     :else (or errback
               (lambda (err)
                 (signal 'infrasonic-api-error
                         (list "Subsonic API request error" err)))))))

(defun infrasonic-get-stream-url (client song-id)
  "Create a streaming URL using CLIENT for song with SONG-ID.
Returns a complete URL for MPV or VLC to directly stream from the server."
  (infrasonic--build-url client
                         "stream"
                         (append (infrasonic--get-auth-params client)
                                 `(("id" . ,song-id)))))

;;;;; Endpoints

;;;; System

;;; ping

(defun infrasonic-ping (client &optional callback errback)
  "Ping the server using CLIENT to check connectivity and authentication.
Returns nil, only displaying a success or failure message.

When non-nil, CALLBACK and ERRBACK will enable an asynchronous request,
and are the callback and error callback functions respectively."
  (if (functionp callback)
      (infrasonic-api-call client "ping" nil callback errback)
    (condition-case err
        (progn
          (infrasonic-api-call client "ping")
          (message "Successfully pinged OpenSubsonic server!"))
      (infrasonic-error
       (message "Failed to ping server: %s" (error-message-string err))))))

;;; getLicense

(defun infrasonic-get-license (client)
  "Get the license status using CLIENT from the OpenSubsonic server.
Returns the parsed license response."
  (alist-get 'license (infrasonic-api-call client "getLicense")))

;;;; Browsing

;;; getGenres

(defun infrasonic-get-genres (client)
  "Get a list of all genres from the server using CLIENT.
Returns a list of genre alists, with (subsonic-type . :genre) appended
to each genre."
  (infrasonic--get-many client
                        "getGenres"
                        '(genres genre)
                        nil
                        :genre))

;;; getArtists

(defun infrasonic-get-artists (client)
  "Get artists index using CLIENT.
Returns an artists alist, which is organised by first letter:
Example:
\(((name . \"#\") (artist . (<list of artists>)))
  ((name . \"a\") (artist . (<list of artists>)))
  ((name . \"b\") (artist . (<list of artists>)))
 ...)

Each artist is standardised as :artist."
  ;; Dont tag with :artist yet, since these are indices
  (let ((indexes (or (infrasonic--get-one client
                                          "getArtists"
                                          'artists)
                     (signal 'infrasonic-api-error
                             (list "Missing getArtists response")))))
    ;; flatten the indexes -> list of artist-lists
    (setf (alist-get 'index indexes)
          (mapcar (lambda (index)
                    (setf (alist-get 'artist index)
                          (infrasonic--standardise-list
                           (infrasonic--ensure-alist-list (alist-get 'artist index))
                           :artist))
                    index)
                  (infrasonic--ensure-alist-list (alist-get 'index indexes))))
    indexes))

(defun infrasonic-get-artists-flat (client)
  "Return a flat list of artists standardised with :artist using CLIENT."
  (let ((indexes (alist-get 'index (infrasonic-get-artists client))))
    (mapcan (lambda (index)
              (alist-get 'artist index))
            indexes)))

;;; getArtist

(defun infrasonic-get-artist (client artist-id)
  "Get information and albums for artist with ARTIST-ID using CLIENT.
Returns an artist alist standardised as :artist.

Each album within the album element is standardised as :album."
  (let ((artist (or (infrasonic--get-one client
                                         "getArtist"
                                         'artist
                                         `(("id" . ,artist-id))
                                         :artist)
                    (signal 'infrasonic-api-error
                            (list "Artist not found" artist-id)))))
    (setf (alist-get 'album artist)
          (infrasonic--standardise-list
           (infrasonic--ensure-alist-list (alist-get 'album artist))
           :album))
    artist))

;;; getAlbum

(defun infrasonic-get-album (client album-id)
  "Get a list of songs for album with ALBUM-ID using CLIENT.
Returns an album alist standardised as :album.

Each song within the song element is standardised as :song."
  (let ((album (or (infrasonic--get-one client
                                        "getAlbum"
                                        'album
                                        `(("id" . ,album-id))
                                        :album)
                   (signal 'infrasonic-api-error
                           (list "Album not found" album-id)))))
    (setf (alist-get 'song album)
          (infrasonic--standardise-list
           (infrasonic--ensure-alist-list (alist-get 'song album))
           :song))
    album))

;;; getSong

(defun infrasonic-get-song (client song-id)
  "Get information for a song with SONG-ID using CLIENT.
Returns the parsed list of song attributes."
  (infrasonic--get-one client
                       "getSong"
                       'song
                       `(("id" . ,song-id))
                       :song))

(defun infrasonic-get-song-async (client song-id callback &optional errback)
  "Asynchronously get information for a song with SONG-ID using CLIENT.
Returns a `plz' process object.

CALLBACK is called on the parsed song alist."
  (infrasonic-api-call client
                       "getSong"
                       `(("id" . ,song-id))
                       (lambda (response)
                         (funcall callback
                                  (infrasonic--standardise (alist-get 'song response)
                                                           :song)))
                       errback))

;;; getArtistInfo2

(defun infrasonic-get-artist-info (client artist-id)
  "Get information about the artist with ARTIST-ID using CLIENT.
Returns the parsed list of artist attributes."
  (let ((artist-info (infrasonic-api-call client
                                          "getArtistInfo2"
                                          `(("id" . ,artist-id)))))
    (alist-get 'artistInfo2 artist-info)))

;;; getAlbumInfo2

(defun infrasonic-get-album-info (client album-id)
  "Get information about the album with ALBUM-ID using CLIENT.
Returns the parsed list of album attributes."
  (let ((album-info (infrasonic-api-call client
                                         "getAlbumInfo2"
                                         `(("id" . ,album-id)))))
    ;; For some reason getAlbumInfo2 has nested albumInfo element (no 2)
    (alist-get 'albumInfo album-info)))

;;; getSimilarSongs2

(defun infrasonic-get-similar-songs (client artist-id &optional n)
  "Get N random songs from ARTIST-ID and from similar artists using CLIENT.
Returns a list of songs."
  (let ((n-songs (or n (infrasonic-client-search-max-results client))))
    (infrasonic--get-many client
                          "getSimilarSongs2"
                          '(similarSongs2 song)
                          `(("id" . ,artist-id)
                            ("count" . ,(number-to-string n-songs)))
                          :song)))

;;; getTopSongs

(defun infrasonic-get-top-songs (client artist-id &optional n)
  "Get ARTIST-ID's N top songs from last.fm using CLIENT.
Returns a list of songs.

Note that the endpoint requires the artist's name, not the ID. This
function uses ARTIST-ID for convenience, and performs a lookup to
convert it to artist name for the request."
  (let ((artist-name (alist-get 'name (infrasonic-get-artist client artist-id)))
        (n-songs (or n (infrasonic-client-search-max-results client))))
    (unless (and (stringp artist-name) (not (string-empty-p artist-name)))
      (signal 'infrasonic-error (list "Artist name is missing" artist-id)))
    (infrasonic--get-many client
                          "getTopSongs"
                          '(topSongs song)
                          `(("artist" . ,artist-name)
                            ("count" . ,(number-to-string n-songs)))
                          :song)))

;;;; Album/song lists

;;; getAlbumList2

(defun infrasonic-get-album-list (client type &optional n offset)
  "Return a list of N albums according to TYPE using CLIENT.
Returns a list of albums. Number returned is dictated by either N, or
the CLIENT's default max search results by default.

TYPE may be:
- A genre string, for example: \"Rock\".
- `:random': Random albums.
- `:newest': Newest albums by release date.
- `:frequent': User's most frequently played albums.
- `:recent': Recently added albums.
- `:starred': Starred albums.
- `:byname': Alphabetically sorted by name.
- `:byartist': Alphabetically sorted by artist.

OFFSET is the depth of results we want to access. That is, return
results from OFFSET to (or N max-search-results) + OFFSET."
  (let* ((n-albums (or n (infrasonic-client-search-max-results client)))
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
                    `(("genre" . ,type)))
                  (when offset
                    `(("offset" . ,(number-to-string offset)))))))
    (infrasonic--get-many client
                          "getAlbumList2"
                          '(albumList2 album)
                          params
                          :album)))

;;; getRandomSongs

(defun infrasonic-get-random-songs (client &optional n offset)
  "Fetch N random songs from the server using CLIENT.
Returns a N length list of parsed JSON songs.

Uses OpenSubsonic API's \"getRandomSongs\" with N as the \"size\".

OFFSET is the depth of results we want to access. That is, return
results from OFFSET to (or N max-search-results) + OFFSET."
  (let ((n-songs (or n (infrasonic-client-search-max-results client))))
    (infrasonic--get-many client
                          "getRandomSongs"
                          '(randomSongs song)
                          (append
                           `(("size" . ,(number-to-string n-songs)))
                           (when offset
                             `(("offset" . ,(number-to-string offset)))))
                          :song)))

;;; getSongsByGenre

(defun infrasonic-get-songs-by-genre (client genre &optional n offset)
  "Get N songs in GENRE using CLIENT.
Returns a list of songs.

OFFSET is the depth of results we want to access. That is, return
results from OFFSET to (or N max-search-results) + OFFSET."
  (let ((n-songs (or n (infrasonic-client-search-max-results client))))
    (infrasonic--get-many client
                          "getSongsByGenre"
                          '(songsByGenre song)
                          (append
                           `(("genre" . ,genre)
                             ("count" . ,(number-to-string n-songs)))
                           (when offset
                             `(("offset" . ,(number-to-string offset)))))
                          :song)))

;;;; getNowPlaying

(defun infrasonic-get-now-playing (client)
  "Get the currently playing songs from all users using CLIENT.
Returns a list of alists:
\(((username . \"username1\") (name . \"song name\") ...)
  ((username . \"username2\") ...)
  ...)"
  (infrasonic--get-many client
                        "getNowPlaying"
                        '(nowPlaying entry)
                        nil
                        :song))

;;; Playlists

(defun infrasonic-get-playlists (client)
  "Fetch all of the user's playlists from the server using CLIENT.
Returns an alist mapping playlist names to their IDs: ((name . id) ...)."
  (when-let* ((items (infrasonic--get-many client
                                           "getPlaylists"
                                           '(playlists playlist)
                                           nil
                                           :playlist)))
    (mapcar (lambda (item)
              (cons (alist-get 'name item)
                    (format "%s" (alist-get 'id item))))
            items)))

(defun infrasonic-get-playlist-songs (client playlist-id)
  "Fetch all songs in playlist with PLAYLIST-ID using CLIENT.
Returns a list of parsed JSON songs."
  (infrasonic--get-many client
                        "getPlaylist"
                        '(playlist entry)
                        `(("id" . ,playlist-id))
                        :song))

;;; Star

(defun infrasonic-get-starred-songs (client)
  "Fetch all starred songs from the server using CLIENT.
Returns a list of parsed JSON songs."
  (infrasonic--get-many client
                        "getStarred2"
                        '(starred2 song)
                        nil
                        :song))

;;;; Setters

;;; Star

(defun infrasonic-star (client item-id star-p &optional callback errback)
  "Set ITEM-ID's star status according to STAR-P using CLIENT.
Returns the parsed API response.

ITEM-ID may be an artist, album or song ID.

Send a request to the \"star\" or \"unstar\" Subsonic endpoints, star (when
STAR-P is non-nil) or unstar ITEM-ID.
CALLBACK is passed to `infrasonic-api-call' and is evaluated on the response
data."
  (infrasonic-api-call client
                       (if star-p "star" "unstar")
                       `(("id" . ,item-id))
                       callback errback))

;;; Rating

(defun infrasonic-set-rating (client item-id rating &optional callback errback)
  "Set ITEM-ID's rating to RATING (0-5) using CLIENT.
Returns the parsed API response.

RATING must be an integer between 0 and 5 inclusive.
A RATING of 0 removes the rating.

CALLBACK and ERRBACK are optional parameters enabling asynchronous requests."
  (unless (and (integerp rating) (<= 0 rating 5))
    (signal 'infrasonic-error (list "Rating must be integer 0-5" rating)))
  (infrasonic-api-call client
                       "setRating"
                       `(("id" . ,item-id)
                         ("rating" . ,(number-to-string rating)))
                       callback errback))

;;; Bookmark

(defun infrasonic-create-bookmark (client song-id position &optional callback errback)
  "Create or update a bookmark for song with SONG-ID at POSITION using CLIENT.
Returns the parsed API response, which will be an empty
\"<subsonic-response>\" element on success.

Sends a request to the \"createBookmark\" endpoint. If CALLBACK is
non-nil, request will be asynchronous and CALLBACK will be evaluated on
the response data."
  (infrasonic-api-call client
                       "createBookmark"
                       `(("id" . ,song-id)
                         ("position" . ,(format "%s" position)))
                       callback errback))

(defun infrasonic-delete-bookmark (client song-id &optional callback errback)
  "Delete the bookmark on song with SONG-ID using CLIENT.
Returns the parsed API response.

Sends a request to the \"deleteBookmark\" endpoint. If CALLBACK is
non-nil, request will be asynchronous and CALLBACK will be evaluated on
the response data."
  (infrasonic-api-call client
                       "deleteBookmark"
                       `(("id" . ,song-id))
                       callback errback))

(defun infrasonic-get-bookmarks (client &optional callback errback)
  "Gets all bookmarks for the user using CLIENT.
Returns the parsed API response.

Sends a request to the \"getBookmarks\" endpoint. If CALLBACK is
non-nil, request will be asynchronous and CALLBACK will be evaluated on
the response data, ERRBACK will evaluate if an error is returned."
  (infrasonic-api-call client
                       "getBookmarks"
                       nil
                       callback errback))

;;; Scrobble

(defun infrasonic-scrobble (client song-id status &optional callback errback)
  "Scrobble STATUS for the song with SONG-ID to the Subsonic API using CLIENT.
Returns the parsed API response.

STATUS may be either `:playing' or `:finished'. If CALLBACK is
non-nil, request will be asynchronous and CALLBACK will be evaluated on
the response data."
  (let* ((submission (pcase status
                       (:playing "false")
                       (:finished "true")
                       (_ (signal 'infrasonic-error (list "Status invalid. Must be either `:playing' or `:finished'"
                                                          status)))))
         (params `(("id" . ,song-id)
                   ("submission" . ,submission))))
    (infrasonic-api-call client
                         "scrobble"
                         params
                         callback errback)))

;;; Search

(defun infrasonic-search (client query &optional offset)
  "Search for QUERY at \"search3\" endpoint for artists, albums and songs CLIENT.
Returns a flat list of items:
\(((subsonic-type . :artist) (name . ...) ...)
  ((subsonic-type . :album) (name . ...) ...)
  ((subsonic-type . :song) (name . ...) ...)
  ((subsonic-type . :song) (name . ...) ...))

Each item is a list with an added element `subsonic-type', and songs
have the `name' element added.

OFFSET is the depth of results we want to access. That is, return
results from OFFSET to (or N max-search-results) + OFFSET."
  (let* ((max-results (number-to-string (/ (infrasonic-client-search-max-results client) 3)))
         (params (append
                  `(("query" . ,query)
                    ("artistCount" . ,max-results)
                    ("albumCount" . ,max-results)
                    ("songCount" . ,max-results))
                  (when offset
                    `(("offset" . ,(number-to-string offset))))))
         (response (infrasonic-api-call client "search3" params))
         (result (alist-get 'searchResult3 response))
         (artists (infrasonic--ensure-alist-list (alist-get 'artist result)))
         (albums (infrasonic--ensure-alist-list (alist-get 'album result)))
         (songs (infrasonic--ensure-alist-list (alist-get 'song result))))
    (append
     (infrasonic--standardise-list artists :artist)
     (infrasonic--standardise-list albums :album)
     (infrasonic--standardise-list songs :song))))

(defun infrasonic-search-songs (client query n)
  "Search the server for N songs matching QUERY using CLIENT.
Returns a list of parsed JSON songs.

Uses the Subsonic API's \"search3\" endpoint with QUERY as the search query."
  (let ((n-songs (or n (infrasonic-client-search-max-results client))))
    (infrasonic--get-many client
                          "search3"
                          '(searchResult3 song)
                          `(("query" . ,query)
                            ("songCount" . ,(number-to-string n-songs)))
                          :song)))

;;; Bulk get songs

(defun infrasonic-get-all-songs (client item-id level)
  "Fetch all songs under item with ITEM-ID using CLIENT.
Returns a list of parsed JSON songs.

ITEM-ID may be an artist or album ID.

LEVEL determines what level of the hierarchy we are on:
- :artist: fetches all albums, then all songs by that artist.
- :album: fetches all songs on the album."
  (pcase level
    (:artist
     (let ((albums (alist-get 'album (infrasonic-get-artist client item-id))))
       (mapcan (lambda (album)
                 (infrasonic-get-all-songs client (alist-get 'id album) :album))
               albums)))
    (:album
     (alist-get 'song (infrasonic-get-album client item-id)))))

;;;; Write requests

(defun infrasonic-create-playlist (client song-ids name &optional callback errback)
  "Create a playlist with NAME containing SONG-IDS on the server using CLIENT.
Returns the newly created playlist.

SONG-IDS should be a list of strings.
NAME should be a string.

This function uses a POST request since large playlists can return HTTP error
414."
  ;; we have to pass one songId per song
  (let* ((body-list (cons `("name" ,name)
                          (mapcar (lambda (id)
                                    (list "songId" id))
                                  song-ids)))
         (body-str (url-build-query-string body-list nil t))
         (playlist (infrasonic-api-call client
                                        "createPlaylist"
                                        nil
                                        callback errback
                                        body-str)))
    (if playlist
        (progn
          (message "Playlist '%s' was created with '%d' songs."
                   name (length song-ids))
          playlist)
      (signal 'infrasonic-error (list "Playlist was not created.")))))

(defun infrasonic-delete-playlist (client playlist-id &optional callback errback)
  "Delete a Subsonic playlist with PLAYLIST-ID using CLIENT.

Providing CALLBACK and ERRBACK will make the API call asynchronous."
  (when (infrasonic-api-call client
                             "deletePlaylist"
                             `(("id" . ,playlist-id))
                             callback errback)
    (message "Playlist deleted.")))

(defun infrasonic-update-playlist (client playlist-id song-ids &optional name comment public-p callback errback)
  "Update playlist with PLAYLIST-ID on the server using CLIENT.
Returns the updated playlist.

SONG-IDS is be a list of song IDs to include in the playlist.

Optional arguments:
- NAME is a string to set the playlist's name
- COMMENT is a string to set the playlist's comment.
- PUBLIC-P should the playlist be public?
- CALLBACK and ERRBACK will make the API call asynchronous.

This function uses a POST request since large playlists can return HTTP error
414.

The \"updatePlaylist\" will wipe all songs when song-ids is set to nil
in the URL. This is probably best handled by \"deletePlaylist\" so we
ensure that \"song-ids=nil\" is never passed to the endpoint. However,
passing nil to this function as SONG-IDS, will NOT send the \"song-ids\"
param, and will allow simple renaming of the playlist."
  (let* ((body-list (append (list `("id" ,playlist-id))
                            (when name (list `("name" ,name)))
                            (when comment (list `("comment" ,comment)))
                            (when public-p (list '("public" "true")))
                            (when song-ids
                              (mapcar (lambda (id) (list "songId" id))
                                      song-ids))))
         (body-list-filt (delq nil body-list))
         (body-str (url-build-query-string body-list-filt nil t)))
    (infrasonic-api-call client
                         "updatePlaylist"
                         nil
                         callback errback
                         body-str)))

;;; Download

(defun infrasonic--get-art-url (client item-id &optional size)
  "Return the art URL for ITEM-ID using CLIENT.

SIZE is the size of the cover art."
  (let ((art-size (or size (infrasonic-client-art-size client))))
    (infrasonic--build-url client
                           "getCoverArt"
                           (append (infrasonic--get-auth-params client)
                                   `(("id" . ,item-id)
                                     ("size" . ,(number-to-string art-size)))))))

(defun infrasonic--download (client url target-file &optional callback errback queue)
  "Asynchronously download music or art from URL to TARGET-FILE using CLIENT.
Returns the `plz-queue' object when QUEUE is non-nil. Returns the `plz'
process object otherwise.

Downloads object to a temporary directory, and moves to TARGET-FILE
after successful. On failure, the function cleans up the failed download.

CALLBACK is called on the newly-downloaded filepath.

ERRBACK is called on failure.

If QUEUE is non-nil, add download to the QUEUE."
  (let ((temp-file (make-temp-name (expand-file-name "infrasonic-part-"
                                                     temporary-file-directory))))
    (make-directory (file-name-directory (expand-file-name target-file)) t)
    (infrasonic--request
     queue
     'get
     url
     :as `(file ,temp-file)
     :then (lambda (_)
             (condition-case err
                 (progn
                   (rename-file temp-file target-file t)
                   (when callback (funcall callback target-file)))
               (error
                (when (file-exists-p temp-file) (delete-file temp-file))
                (if errback
                    (funcall errback err)
                  (signal 'infrasonic-filesystem-error
                          (list "Failed to finalise download" err target-file))))))
     :else (lambda (err)
             ;; Clean temp files
             (when (file-exists-p temp-file) (delete-file temp-file))
             (if errback
                 (funcall errback err)
               (signal 'infrasonic-api-error
                       (list "Music download failed" err))))
     ;; 5 minute timeout
     :timeout (infrasonic-client-timeout client))))

(defun infrasonic-download-art (client item-id target-file &optional size callback errback queue)
  "Download cover art for ITEM-ID and write to TARGET-FILE using CLIENT.
Returns a `plz-queue' object. Cancel with `plz-clear'.

SIZE overrides the default art size set in `infrasonic-client'.

CALLBACK is called when the art is downloaded. This may be used to
display the image in TARGET-FILE somewhere in Emacs once it's finished
downloading."
  (let* ((url (infrasonic--get-art-url client item-id size))
         (queue-limit (infrasonic-client-queue-limit client))
         (que (or queue (make-plz-queue :limit queue-limit))))
    (infrasonic--download client url target-file callback errback que)
    (unless queue (plz-run que))
    que))

(defun infrasonic-download-music (client item-id out-dir &optional callback errback queue)
  "Asynchronously download song with ITEM-ID to OUT-DIR using CLIENT.
Returns a `plz-queue' object. Cancel with `plz-clear'.

Downloads ITEM-ID to a temporary directory, and moves to OUT-DIR, with the
title of the item as the filename. The filetype is determined by the filetype
of the media on the OpenSubsonic server.
after successful. On failure, the function cleans up the failed download.

CALLBACK is called on the newly-downloaded filepath.

ERRBACK is passed to both `infrasonic-get-song-async' and
`infrasonic--download'."
  (let* ((auth-params (infrasonic--get-auth-params client))
         (queue-limit (infrasonic-client-queue-limit client))
         (que (or queue (make-plz-queue :limit queue-limit))))
    ;; to keep both getSong and download async, can just use callback
    (infrasonic-api-call client
                         "getSong"
                         `(("id" . ,item-id))
                         (lambda (resp)
                           (let* ((song (infrasonic--standardise (alist-get 'song resp) :song))
                                  (suffix (alist-get 'suffix song))
                                  (title (infrasonic--safe-filename (alist-get 'name song)))
                                  (filename (format "%s.%s" title suffix))
                                  (full-path (expand-file-name filename out-dir))
                                  (url (infrasonic--build-url client
                                                              "download"
                                                              (append auth-params
                                                                      `(("id" . ,item-id))))))
                             (infrasonic--download client
                                                   url
                                                   full-path
                                                   callback
                                                   errback
                                                   que)))
                         errback
                         nil
                         que)
    (unless queue (plz-run que))
    que))

;;; High-level stuff

(defun infrasonic-children (client id level)
  "Fetch \"nodes\" for directory hierarchy LEVEL and ID using CLIENT.
Returns a list of alists, each alist representing children of ID.

This is intended as a helper function for accessing the OpenSubsonic
server as a directory structure.

LEVEL determines the endpoint to use, and may be one of:
- :artists: Returns top-level view of all artists using endpoint \"getArtists\".
- :artist: Returns albums for an artist using \"getArtist\".
- :album: Returns songs in an album using \"getAlbum\"."
  (let ((items
         (pcase level
           (:artists (infrasonic-get-artists-flat client))
           (:artist (alist-get 'album (infrasonic-get-artist client id)))
           (:album (alist-get 'song (infrasonic-get-album client id))))))
    items))

(provide 'infrasonic)

;;; infrasonic.el ends here
