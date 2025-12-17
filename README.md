# Infrasonic

infrasonic.el is an Emacs library for interacting with OpenSubsonic-compatible
music servers such as Gonic, Navidrome, etc.

It handles authentication, request signing, and JSON parsing. Authentication is
handled with auth-source using the OpenSubsonic token/salt authentication
method.

`infrasonic` also ensures consistency between tracks, albums and artists by
adding a "name" element to tracks. A "subsonic-type" element is added to, which
indicates if the item is a track, album or artist.

## Requirements
- Emacs 30.1 or higher.
- `plz.el` (0.9 or higher) for HTTP requests.
- An OpenSubsonic-compatible server.

## Installation
Clone the repository and add it to your load-path:

```emacs-lisp
(add-to-list 'load-path "/path/to/infrasonic.el")
(require 'infrasonic)
```

`straight.el`:
```emacs-lisp
(straight-use-package
 '(infrasonic :type git :host github :repo "username/infrasonic.el"))
```

`elpaca`
```emacs-lisp
(use-package infrasonic
  :ensure '(infrasonic :repo "~/repos/infrasonic/")
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

Test connection using `M-x infrasonic-ping-server`

`infrasonic` is designed to be used as a backend for other multimedia packages (like listen.el).

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

## API Implementation Status
### 1.0.0
- [ ] download
- [x] getCoverArt
- [ ] getIndexes
- [ ] getLicense
- [ ] getMusicDirectory
- [ ] getMusicFolders
- [ ] getNowPlaying
- [x] getPlaylist
- [x] getPlaylists
- [x] ping
- [ ] search
- [x] stream
### 1.1.0
- [ ] changePassword
- [ ] createUser
### 1.2.0
- [ ] addChatMessage
- [x] createPlaylist
- [x] deletePlaylist
- [ ] getAlbumList
- [ ] getChatMessages
- [ ] getLyrics
- [x] getRandomSongs
- [ ] jukeboxControl
### 1.3.0
- [ ] deleteUser
- [ ] getUser
### 1.4.0
- [ ] search2
### 1.5.0
- [x] scrobble
### 1.6.0
- [ ] createShare
- [ ] deleteShare
- [ ] getPodcasts
- [ ] getShares
- [ ] setRating
- [ ] updateShare
### 1.8.0
- [x] getAlbum
- [ ] getAlbumList2
- [x] getArtist
- [x] getArtists
- [ ] getAvatar
- [ ] getSong
- [ ] getStarred
- [x] getStarred2
- [ ] getUsers
- [ ] getVideos
- [ ] hls
- [x] search3
- [x] star
- [x] unstar
- [ ] updatePlaylist
### 1.9.0
- [ ] createBookmark
- [ ] createPodcastChannel
- [ ] deleteBookmark
- [ ] deletePodcastChannel
- [ ] deletePodcastEpisode
- [ ] downloadPodcastEpisode
- [ ] getBookmarks
- [ ] getGenres
- [ ] getInternetRadioStations
- [ ] getSongsByGenre
- [ ] refreshPodcasts
### 1.10.1
- [ ] updateUser
### 1.11.0
- [ ] getArtistInfo
- [ ] getArtistInfo2
- [ ] getSimilarSongs
- [ ] getSimilarSongs2
### 1.12.0(100.0%)
- [ ] getPlayQueue
- [ ] savePlayQueue
### 1.13.0
- [ ] getNewestPodcasts
- [ ] getTopSongs
### 1.14.0
- [ ] getAlbumInfo
- [ ] getAlbumInfo2
- [ ] getCaptions
- [ ] getVideoInfo
### 1.15.0
- [ ] getScanStatus
- [ ] startScan
### 1.16.0
- [ ] createInternetRadioStation
- [ ] deleteInternetRadioStation
- [ ] updateInternetRadioStation
