# Infrasonic

- [Installation](#installation)
- [Usage](#usage)
- [Functions](#functions)
- [API Implementation Status](#api implementation status)

`infrasonic` is an Emacs library for interacting with OpenSubsonic-compatible
music servers such as Gonic, Navidrome, etc.

It handles authentication, request signing, and JSON parsing. Authentication is
handled with `auth-source` using the OpenSubsonic token/salt authentication method.

`infrasonic` also ensures consistency between tracks, albums and artists by
adding a "name" element to tracks. A "subsonic-type" element is added too,
which indicates if the item is a track, album or artist. Other than this, the
API request is untouched (other than the JSON -> list parsing).

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
1. Server Settings

Set your server's url and protocol:
```emacs-lisp
(setq infrasonic-url "music.example.com"  ; or "192.168.1.50:4533"
      infrasonic-protocol "https"         ; "http" or "https"
      infrasonic-user-agent "infrasonic")
```

2. Authentication

`infrasonic` uses Emacs' built-in `auth-source` library to handle credentials.
Add a line to your ~/.authinfo file matching your infrasonic-url.

```txt
machine music.example.com login my_username password my_secret_password
```

3. Low-level Functions

Test connection using `M-x infrasonic-ping`.

`infrasonic` is designed to be used as a backend for other multimedia packages
(like listen.el).

Some examples:

``` emacs-lisp
;; Search for a query (returns artists, albums, and tracks)
(infrasonic-search "Daft Punk")

;; Get all playlists
(infrasonic-get-playlists)

;; Get 50 random tracks
(infrasonic-get-random-tracks 50)

;; Scrobble a track (ID "123") as "Now Playing"
(infrasonic-scrobble "123" :playing)

;; Star a track
(infrasonic-star "123" t)

;; API call with callback (callback enables async requests)
(infrasonic-api-call "ping" nil
                     (lambda (data)
                     (message "Server says: %s" (alist-get 'status data))))
```

4. High-level functions

`infrasonic` also provides some higher-level functions:

```emacs-lisp
;; Get all tracks under an artist or album
(infrasonic-get-all-tracks "123" :artist)
```

## Functions:

<details><summary><b>(album), (artist), (track), etc. refer to the full parsed list construct.</b></summary>

``` emacs-lisp
;; This is the parsed JSON returned from `infrasonic-search' to a gonic server.
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
 ((subsonic-type . :track)
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

| Function name                    | API endpoint                        | Returns                                                   |
|----------------------------------|-------------------------------------|-----------------------------------------------------------|
| `infrasonic-get-stream-url`      | `stream`                            | Complete URL string for streaming                         |
| `infrasonic-ping`                | `ping`                              | `nil` (Displays success/failure message)                  |
| `infrasonic-get-artists`         | `getArtists`                        | Flat list of artists: `((artist1) (artist2) ...)`         |
| `infrasonic-get-artist`          | `getArtist`                         | Flat list of albums: `((album1) (album2) ...)`            |
| `infrasonic-get-album`           | `getAlbum`                          | Flat list of tracks: `((track1) (track2) ...)`            |
| `infrasonic-get-playlists`       | `getPlaylists`                      | Alist of names/IDs: `((name . id) ...)`                   |
| `infrasonic-get-playlist-tracks` | `getPlaylist`                       | List of tracks: `((track1) (track2) ...)`                 |
| `infrasonic-get-starred-tracks`  | `getStarred2`                       | List of tracks: `((track1) (track2) ...)`                 |
| `infrasonic-star`                | `star` / `unstar`                   | Parsed response                                           |
| `infrasonic-create-bookmark`     | `createBookmark`                    | Parsed response                                           |
| `infrasonic-delete-bookmark`     | `deleteBookmark`                    | Parsed response                                           |
| `infrasonic-get-bookmarks`       | `getBookmarks`                      | Parsed response                                           |
| `infrasonic-scrobble`            | `scrobble`                          | Parsed response                                           |
| `infrasonic-get-random-tracks`   | `getRandomSongs`                    | List of n tracks: `((track1) ...)`                        |
| `infrasonic-search`              | `search3`                           | Flat list of results: `((artist1) (album1) (track1) ...)` |
| `infrasonic-search-tracks`       | `search3`                           | List of tracks: `((track1) (track2) ...)`                 |
| `infrasonic-get-all-tracks`      | `getArtist` / `getAlbum`            | List of tracks: `((track1) (track2) ...)`                 |
| `infrasonic-create-playlist`     | `createPlaylist`                    | Playlist object: `((id . "1") (name . "foo") ...)`        |
| `infrasonic-delete-playlist`     | `deletePlaylist`                    | `t` (if successful)                                       |
| `infrasonic-get-art-url`         | Builds `getCoverArt` URL            | Complete URL string for image resource                    |
| `infrasonic-get-art`             | `getCoverArt`                       | File path to downloaded image                             |
| `infrasonic-download`            | `download`                          | File path to downloaded music                             |
| `infrasonic-children`            | `getArtists`/`getArtist`/`getAlbum` | List of items (artists, albums, or tracks)                |

## API Implementation Status

`infrasonic` only plans to support ID3 endpoints (e.g. `search` and `search2`
will likely not be implemented). `infrasonic` will only support music and has
no plans to support podcasts or music videos.

<details><summary><b>Click to expand the API implementation checklist</b></summary>
| *1.0.0*                    |                                  |
| download                   |                                  |
| getCoverArt                | `infrasonic-get-art(-url)`       |
| getIndexes                 |                                  |
| getLicense                 |                                  |
| getMusicDirectory          |                                  |
| getMusicFolders            |                                  |
| getNowPlaying              |                                  |
| getPlaylist                | `infrasonic-get-playlist-tracks` |
| getPlaylists               | `infrasonic-get-playlists`       |
| ping                       | `infrasonic-ping-server`         |
| search                     |                                  |
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
| getRandomSongs             | `infrasonic-get-random-tracks`   |
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
| getSong                    |                                  |
| getStarred                 | Not planned                      |
| getStarred2                | `infrasonic-get-starred-tracks`  |
| getUsers                   | Not planned                      |
| getVideos                  | Not planned                      |
| hls                        |                                  |
| search3                    | `infrasonic-search`              |
| star                       | `infrasonic-star`                |
| unstar                     | `infrasonic-star`                |
| updatePlaylist             |                                  |
| *1.9.0*                    |                                  |
| createBookmark             |                                  |
| createPodcastChannel       | Maybe?                           |
| deleteBookmark             |                                  |
| deletePodcastChannel       | Maybe?                           |
| deletePodcastEpisode       | Maybe?                           |
| downloadPodcastEpisode     | Maybe?                           |
| getBookmarks               |                                  |
| getGenres                  |                                  |
| getInternetRadioStations   | Not planned                      |
| getSongsByGenre            |                                  |
| refreshPodcasts            | Maybe?                           |
| *1.10.1*                   |                                  |
| updateUser                 | Not planned                      |
| *1.11.0*                   |                                  |
| getArtistInfo              |                                  |
| getArtistInfo2             |                                  |
| getSimilarSongs            |                                  |
| getSimilarSongs2           |                                  |
| *1.12.0                    |                                  |
| getPlayQueue               |                                  |
| savePlayQueue              |                                  |
| *1.13.0*                   |                                  |
| getNewestPodcasts          | Maybe                            |
| getTopSongs                |                                  |
| *1.14.0*                   |                                  |
| getAlbumInfo               | Not planned                      |
| getAlbumInfo2              |                                  |
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
