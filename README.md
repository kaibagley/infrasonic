# Infrasonic

- [Installation](#installation)
- [Usage](#usage)
- [Functions](#functions)
- [API Implementation Status](#api implementation status)

`infrasonic` is an Emacs library for interacting with OpenSubsonic-compatible
music servers such as Gonic, Navidrome, etc. It is designed for use in other
Emacs packages wishing to implement OpenSubsonic compatibility.

It handles authentication, request signing, and JSON parsing. Authentication is
handled with `auth-source` using the OpenSubsonic token/salt authentication method.

`infrasonic` also ensures consistency between songs, albums and artists by
adding a "name" element to songs. A "subsonic-type" element is added too,
which indicates if the item is a song, album or artist. Other than this, the
API request is untouched (other than the JSON -> alist parsing).

## Requirements
- Emacs 30.1 or higher for built-in JSON support.
- `plz` (0.9 or higher) for HTTP requests.
- An OpenSubsonic-compatible server.

## Installation

Hopefully at some point, this package will be available on a package
repository. For now:

Manual:
Clone the repository and add it to your load-path:

```emacs-lisp
(add-to-list 'load-path "/path/to/infrasonic_repo")
(require 'infrasonic)
```

`straight`:

```emacs-lisp
(straight-use-package
 '(infrasonic :type git :host github :repo "kaibagley/infrasonic"))
```

`elpaca`

```emacs-lisp
(use-package infrasonic
  :ensure '(infrasonic :repo "kaibagley/infrasonic")
  ...)
```

## Usage

`infrasonic` requires you to create a *client*, which is simply a plist
containing a few important details. The client is passed as the first argument
to the `infrasonic` API functions, allowing multiple clients to be used at
once.

1. Authentication

`infrasonic` uses `auth-source` for secrets. Add a line to your `~/.authinfo`
containing your OpenSubsonic server's URL.

```text
machine music.example.com login my_username password my_secret_password
```

2. Client

In your package, a client must be created for each server you wish to access.
It can be done as follows:

```emacs-lisp
;; (default values)
(defvar my-music-client
  (infrasonic-make-client
   :url                ; (no default)     FQDN for OpenSubsonic server
   :protocol           ; ("https")        "http" or "https"
   :user-agent         ; ("infrasonic")   Name of your player
   ;; Implemented Open Subsonic REST API version.
   ;; See subsonic.org for the mapping between OpenSubsonic version and REST API version.
   :api-version        ; ("1.16.1")
   :queue-limit        ; (5)              Limit of concurrent downloads of art and music
   :timeout            ; (300)            HTTP request timeout for downloads of art and music
   :art-size           ; (64)             Edge size in pixels to dowload cover art
   :search-max-results ; (200))           Max results to return in query searches
```

3. Low-level Functions

Below are some basic examples of the API functions

```emacs-lisp
;; Test connection
(infrasonic-ping my-music-client)

;; Search for a query (returns artists, albums, and songs)
(infrasonic-search my-music-client "Bilmuri")

;; Get all playlists
(infrasonic-get-playlists my-music-client)

;; Get 50 random songs
(infrasonic-get-random-songs my-music-client 50)

;; Scrobble a song (ID "id-123") as "Now Playing"
(infrasonic-scrobble my-music-client "id-123" :playing)

;; Star a song
(infrasonic-star my-music-client "id-123" t)

;; API call with callback and errback (callback enables async requests)
;; Most functions support callbacks for asynchronous execution
(infrasonic-api-call my-music-client
                     "ping"         ; Endpoint
                     nil            ; Parameters
                     (lambda (resp) ; Callback function
                      (message "Server says: %s" (alist-get 'status resp)))
                     (lambda (err)  ; Error callback function
                      (user-error "Server errored: %S" err)))
```

4. High-level functions

`infrasonic` also provides some higher-level functions:

```emacs-lisp
;; Get all songs recursively under an artist or album
(infrasonic-get-all-songs my-music-client "id-123" :artist)
```

## Functions:

<details><summary><b>Click for a full data structure example</b></summary>

(album), (artist), (song), etc. refer to the full parsed list construct.

``` emacs-lisp
;; This is an example of the full parsed JSON returned from `infrasonic-search'
;; to my gonic server.
(((subsonic-type . :artist)
  (id . "ar-461")
  (name . "The Human Abstract")
  (albumCount . 5))
 ((subsonic-type . :album)
  (id . "al-6872")
  (created . "2024-03-14T19:47:03.288353577+08:00")
  (artistId . "ar-479")
  (artist . "Tigercub")
  (artists ((id . "ar-479") (name . "Tigercub")))
  (displayArtist . "Tigercub")
  (title . "Abstract Figures in the Dark")
  (album . "Abstract Figures in the Dark")
  (coverArt . "al-6872")
  (name . "Abstract Figures in the Dark")
  (songCount . 0)
  (duration . 0)
  (playCount . 0)
  (genre . "Rock")
  (genres ((name . "Rock")))
  (year . 2016)
  (isCompilation)
  (releaseTypes "Album"))
 ((subsonic-type . :song)
  (name . "The Abstract of a Planet in Resolve (instrumental)")
  (id . "tr-7252")
  (album . "To Speak, To Listen")
  (albumId . "al-2004")
  (artist . "Eidola")
  (artistId . "ar-127")
  (artists ((id . "ar-127") (name . "Eidola")))
  (displayArtist . "Eidola")
  (albumArtists ((id . "ar-127") (name . "Eidola")))
  (displayAlbumArtist . "Eidola")
  (bitRate . 320)
  (contentType . "audio/mpeg")
  (coverArt . "al-2004")
  (created . "2025-12-09T20:59:09.034293055+08:00")
  (duration . 159)
  (genre . "Experimental;Experimental Rock;Post-Hardcore;Progressive Metal")
  (genres ((name . "Experimental;Experimental Rock;Post-Hardcore;Progressive Metal")))
  (isDir)
  (isVideo)
  (parent . "al-2004")
  (path . "Eidola/Eidola - Album - 2017 - To Speak To Listen/0101 - The Abstract of a Planet in Resolve instrumental.mp3")
  (size . 6371345)
  (suffix . "mp3")
  (title . "The Abstract of a Planet in Resolve (instrumental)")
  (track . 1)
  (discNumber . 1)
  (type . "music")
  (year . 2017)
  (musicBrainzId . "9b2c86c9-e2f5-476a-ba9b-7c65282fc954")
  (replayGain)))
```

</details>

| Function name                    | API endpoint                        | Returns                                                  |
|----------------------------------|-------------------------------------|----------------------------------------------------------|
| `infrasonic-get-stream-url`      | `stream`                            | Complete URL string for streaming                        |
| `infrasonic-ping`                | `ping`                              | `nil` (Displays success/failure message)                 |
| `infrasonic-get-artists`         | `getArtists`                        | Flat list of artists: `((artist1) (artist2) ...)`        |
| `infrasonic-get-artist`          | `getArtist`                         | Flat list of albums: `((album1) (album2) ...)`           |
| `infrasonic-get-album`           | `getAlbum`                          | Flat list of songs: `((song1) (song2) ...)`              |
| `infrasonic-get-playlists`       | `getPlaylists`                      | Alist of names/IDs: `((name . id) ...)`                  |
| `infrasonic-get-playlist-songs`  | `getPlaylist`                       | List of songs: `((song1) (song2) ...)`                   |
| `infrasonic-get-starred-songs`   | `getStarred2`                       | List of songs: `((song1) (song2) ...)`                   |
| `infrasonic-star`                | `star` / `unstar`                   | Parsed response                                          |
| `infrasonic-create-bookmark`     | `createBookmark`                    | Parsed response                                          |
| `infrasonic-delete-bookmark`     | `deleteBookmark`                    | Parsed response                                          |
| `infrasonic-get-bookmarks`       | `getBookmarks`                      | Parsed response                                          |
| `infrasonic-scrobble`            | `scrobble`                          | Parsed response                                          |
| `infrasonic-get-random-songs`    | `getRandomSongs`                    | List of n songs: `((song1) ...)`                         |
| `infrasonic-search`              | `search3`                           | Flat list of results: `((artist1) (album1) (song1) ...)` |
| `infrasonic-search-songs`        | `search3`                           | List of songs: `((song1) (song2) ...)`                   |
| `infrasonic-get-all-songs`       | `getArtist` / `getAlbum`            | List of songs: `((song1) (song2) ...)`                   |
| `infrasonic-create-playlist`     | `createPlaylist`                    | Playlist object: `((id . "1") (name . "foo") ...)`       |
| `infrasonic-delete-playlist`     | `deletePlaylist`                    | `t` (if successful)                                      |
| `infrasonic-get-genres`          | `getGenres`                         | List of genres: `((genre1) (genre2) ...)`                |
| `infrasonic-update-playlist`     | `updatePlaylist`                    | Parsed response                                          |
| `infrasonic-get-art-url`         | Builds `getCoverArt` URL            | Complete URL string for image resource                   |
| `infrasonic-download-art`        | `getCoverArt`                       | A `plz-queue`                                            |
| `infrasonic-download-music`      | `getSong` and `download`            | A `plz-queue`                                            |
| `infrasonic-children`            | `getArtists`/`getArtist`/`getAlbum` | List of items (artists, albums, or songs)                |

## API Implementation Status

`infrasonic` only plans to support ID3 endpoints (e.g. `search` and `search2`
will likely not be implemented). `infrasonic` will only support music and has
no plans to support podcasts or music videos.

<details><summary><b>Click to expand the API implementation checklist</b></summary>
| *1.0.0*                    |                                  |
| download                   | `infrasonic-download-music`      |
| getCoverArt                | `infrasonic-get-art(-url)`       |
| getIndexes                 |                                  |
| getLicense                 | `infrasonic-get-license`         |
| getMusicDirectory          |                                  |
| getMusicFolders            |                                  |
| getNowPlaying              |                                  |
| getPlaylist                | `infrasonic-get-playlist-songs`  |
| getPlaylists               | `infrasonic-get-playlists`       |
| ping                       | `infrasonic-ping`                |
| search                     | Not planned                      |
| stream                     | `infrasonic-get-stream-url`      |
| *1.1.0*                    |                                  |
| changePassword             | Not planned                      |
| createUser                 | Not planned                      |
| *1.2.0*                    |                                  |
| addChatMessage             | Not planned                      |
| createPlaylist             | `infrasonic-create-playlist`     |
| deletePlaylist             | `infrasonic-delete-playlist`     |
| getAlbumList               | Not planned                      |
| getChatMessages            | Not planned                      |
| getLyrics                  |                                  |
| getRandomSongs             | `infrasonic-get-random-songs`    |
| jukeboxControl             | Not planned                      |
| *1.3.0*                    |                                  |
| deleteUser                 | Not planned                      |
| getUser                    | Not planned                      |
| *1.4.0*                    |                                  |
| search2                    | Not planned                      |
| *1.5.0*                    |                                  |
| scrobble                   | `infrasonic-scrobble`            |
| *1.6.0*                    |                                  |
| createShare                | Not planned                      |
| deleteShare                | Not planned                      |
| getPodcasts                |                                  |
| getShares                  | Not planned                      |
| setRating                  |                                  |
| updateShare                | Not planned                      |
| *1.8.0*                    |                                  |
| getAlbum                   | `infrasonic-get-album`           |
| getAlbumList2              |                                  |
| getArtist                  | `infrasonic-get-artist`          |
| getArtists                 | `infrasonic-get-artists`         |
| getAvatar                  |                                  |
| getSong                    | `infrasonic-get-song`            |
| getStarred                 | Not planned                      |
| getStarred2                | `infrasonic-get-starred-songs`   |
| getUsers                   | Not planned                      |
| getVideos                  | Not planned                      |
| hls                        |                                  |
| search3                    | `infrasonic-search`              |
| star                       | `infrasonic-star`                |
| unstar                     | `infrasonic-star`                |
| updatePlaylist             | `infrasonic-update-playlist`     |
| *1.9.0*                    |                                  |
| createBookmark             | `infrasonic-create-bookmark`     |
| createPodcastChannel       | Maybe?                           |
| deleteBookmark             | `infrasonic-delete-bookmark`     |
| deletePodcastChannel       | Maybe?                           |
| deletePodcastEpisode       | Maybe?                           |
| downloadPodcastEpisode     | Maybe?                           |
| getBookmarks               | `infrasonic-get-bookmarks`       |
| getGenres                  | `infrasonic-get-genres`          |
| getInternetRadioStations   | Not planned                      |
| getSongsByGenre            | `infrasonic-get-songs-by-genre`  |
| refreshPodcasts            | Maybe?                           |
| *1.10.1*                   |                                  |
| updateUser                 | Not planned                      |
| *1.11.0*                   |                                  |
| getArtistInfo              |                                  |
| getArtistInfo2             |                                  |
| getSimilarSongs            |                                  |
| getSimilarSongs2           | `infrasonic-get-similar-songs`   |
| *1.12.0                    |                                  |
| getPlayQueue               |                                  |
| savePlayQueue              |                                  |
| *1.13.0*                   |                                  |
| getNewestPodcasts          | Maybe                            |
| getTopSongs                | `infrasonic-get-top-songs`       |
| *1.14.0*                   |                                  |
| getAlbumInfo               | Not planned                      |
| getAlbumInfo2              | `infrasonic-get-album-info`      |
| getCaptions                | Not planned                      |
| getVideoInfo               | Not planned                      |
| *1.15.0*                   |                                  |
| getScanStatus              | Not planned                      |
| startScan                  | Not planned                      |
| *1.16.0*                   |                                  |
| createInternetRadioStation | Not planned                      |
| deleteInternetRadioStation | Not planned                      |
| updateInternetRadioStation | Not planned                      |
</details>
