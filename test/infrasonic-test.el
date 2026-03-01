;;; infrasonic-test.el --- Tests for infrasonic         -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Kai Bagley <kaibagley+github@proton.mail>
;; Maintainer: Kai Bagley <kaibagley+github@proton.mail>
;; Keywords: multimedia
;; Package-Requires: ((emacs "30") (plz "0.9"))

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

;; Commentary

;; AI SLOP ALERT
;; THESE TESTS ARE WRITTEN BY AN LLM (and reviewed by me, Kai)
;;
;; Tests are a bit boring to write, and LLM's seem pretty good at doing this,
;; and doing them quickly while covering a lot of edge-cases.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'infrasonic)

;;;; Test helpers

(defun infrasonic-test--make-client (&rest overrides)
  "Create a valid test client, optionally merging OVERRIDES."
  (apply #'infrasonic-make-client
         (append (unless (plist-get overrides :url)
                   '(:url "music.example.com"))
                 overrides)))

(defmacro infrasonic-test--with-json-buffer (json-string &rest body)
  "Execute BODY in a temp buffer containing JSON-STRING."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,json-string)
     ,@body))

;;;; Client struct

(ert-deftest infrasonic-test-make-client ()
  "Creating a client with a valid URL should return an infrasonic-client."
  (let ((client (infrasonic-make-client :url "music.example.com")))
    (should (infrasonic-client-p client))
    (should (equal (infrasonic-client-url client) "music.example.com"))))

(ert-deftest infrasonic-test-make-client-custom-fields ()
  "All client fields should be settable at construction."
  (let ((client (infrasonic-make-client
                 :url "music.example.com"
                 :protocol "http"
                 :user-agent "my-player"
                 :api-version "1.15.0"
                 :queue-limit 10
                 :timeout 60
                 :art-size 128
                 :search-max-results 500)))
    (should (equal (infrasonic-client-protocol client) "http"))
    (should (equal (infrasonic-client-user-agent client) "my-player"))
    (should (equal (infrasonic-client-api-version client) "1.15.0"))
    (should (equal (infrasonic-client-queue-limit client) 10))
    (should (equal (infrasonic-client-timeout client) 60))
    (should (equal (infrasonic-client-art-size client) 128))
    (should (equal (infrasonic-client-search-max-results client) 500))))

(ert-deftest infrasonic-test-make-client-defaults ()
  "Client fields should have correct defaults."
  (let ((client (infrasonic-make-client :url "music.example.com")))
    (should (equal (infrasonic-client-protocol client) "https"))
    (should (equal (infrasonic-client-user-agent client) "infrasonic"))
    (should (equal (infrasonic-client-api-version client) "1.16.1"))
    (should (equal (infrasonic-client-queue-limit client) 5))
    (should (equal (infrasonic-client-timeout client) 300))
    (should (equal (infrasonic-client-art-size client) 64))
    (should (equal (infrasonic-client-search-max-results client) 200))))

(ert-deftest infrasonic-test-make-client-missing-url ()
  "Creating a client without a URL should signal an error."
  (should-error (infrasonic-make-client) :type 'infrasonic-error))

(ert-deftest infrasonic-test-make-client-empty-url ()
  "Creating a client with an empty URL should signal an error."
  (should-error (infrasonic-make-client :url "") :type 'infrasonic-error))

(ert-deftest infrasonic-test-make-client-nil-url ()
  "Creating a client with nil URL should signal an error."
  (should-error (infrasonic-make-client :url nil) :type 'infrasonic-error))

(ert-deftest infrasonic-test-make-client-invalid-protocol ()
  "Creating a client with an invalid protocol should signal an error."
  (should-error (infrasonic-make-client :url "music.example.com" :protocol "ftp")
                :type 'infrasonic-error))

(ert-deftest infrasonic-test-make-client-http-protocol ()
  "Creating a client with http protocol should succeed."
  (let ((client (infrasonic-make-client :url "music.example.com" :protocol "http")))
    (should (equal (infrasonic-client-protocol client) "http"))))

(ert-deftest infrasonic-test-client-type-check ()
  "Passing a non-client to cl-check-type should signal a type error."
  (should-error (cl-check-type '(:url "example.com") infrasonic-client)
                :type 'wrong-type-argument))

;;;; Standardisation

(ert-deftest infrasonic-test-standardise-song-with-title ()
  "Standardising a song should use `title' as `name'."
  (let* ((song '((title . "Test Song") (artist . "Test Artist") (id . "tr-1")))
         (result (infrasonic--standardise song :song)))
    (should (eq (alist-get 'subsonic-type result) :song))
    (should (equal (alist-get 'name result) "Test Song"))))

(ert-deftest infrasonic-test-standardise-album-with-name ()
  "Standardising an album should preserve existing `name'."
  (let* ((album '((name . "Test Album") (artist . "Test Artist") (id . "al-1")))
         (result (infrasonic--standardise album :album)))
    (should (eq (alist-get 'subsonic-type result) :album))
    (should (equal (alist-get 'name result) "Test Album"))))

(ert-deftest infrasonic-test-standardise-artist ()
  "Standardising an artist should set type and preserve name."
  (let* ((artist '((name . "Test Artist") (id . "ar-1")))
         (result (infrasonic--standardise artist :artist)))
    (should (eq (alist-get 'subsonic-type result) :artist))
    (should (equal (alist-get 'name result) "Test Artist"))))

(ert-deftest infrasonic-test-standardise-name-fallback-artist ()
  "When no `name' or `title', should fall back to `artist' field."
  (let* ((item '((artist . "Fallback Artist") (id . "1")))
         (result (infrasonic--standardise item :song)))
    (should (equal (alist-get 'name result) "Fallback Artist"))))

(ert-deftest infrasonic-test-standardise-name-fallback-album ()
  "When no `name', `title', or `artist', should fall back to `album' field."
  (let* ((item '((album . "Fallback Album") (id . "1")))
         (result (infrasonic--standardise item :song)))
    (should (equal (alist-get 'name result) "Fallback Album"))))

(ert-deftest infrasonic-test-standardise-name-fallback-nil ()
  "When no name fields exist, `name' should be nil."
  (let* ((item '((id . "1")))
         (result (infrasonic--standardise item :song)))
    (should (null (alist-get 'name result)))))

(ert-deftest infrasonic-test-standardise-name-priority ()
  "Name resolution should prefer `name' over `title' over `artist' over `album'."
  (let* ((item '((name . "N") (title . "T") (artist . "A") (album . "Al")))
         (result (infrasonic--standardise item :song)))
    (should (equal (alist-get 'name result) "N")))
  (let* ((item '((title . "T") (artist . "A") (album . "Al")))
         (result (infrasonic--standardise item :song)))
    (should (equal (alist-get 'name result) "T"))))

(ert-deftest infrasonic-test-standardise-list ()
  "Standardise-list should tag every item in the list."
  (let* ((items (list (list (cons 'name "A") (cons 'id "1"))
                      (list (cons 'name "B") (cons 'id "2"))
                      (list (cons 'name "C") (cons 'id "3"))))
         (result (infrasonic--standardise-list items :album)))
    (should (equal (length result) 3))
    (dolist (item result)
      (should (eq (alist-get 'subsonic-type item) :album)))))

(ert-deftest infrasonic-test-standardise-list-empty ()
  "Standardise-list on an empty list should return nil."
  (should (null (infrasonic--standardise-list nil :song))))

;;;; alist-p

(ert-deftest infrasonic-test-alist-p-valid ()
  "Should detect a standard alist."
  (should (infrasonic--alist-p '((a . 1) (b . 2)))))

(ert-deftest infrasonic-test-alist-p-single-pair ()
  "Should detect a single-pair alist."
  (should (infrasonic--alist-p '((a . 1)))))

(ert-deftest infrasonic-test-alist-p-nil ()
  "Nil should not be an alist."
  (should-not (infrasonic--alist-p nil)))

(ert-deftest infrasonic-test-alist-p-flat-list ()
  "A flat list of symbols should not be an alist."
  (should-not (infrasonic--alist-p '(a b c))))

(ert-deftest infrasonic-test-alist-p-nested-list ()
  "A list of alists (nested conses) should not be detected as a single alist."
  (should-not (infrasonic--alist-p '(((a . 1)) ((b . 2))))))

;;;; ensure-alist-list

(ert-deftest infrasonic-test-ensure-alist-list-nil ()
  "Nil should return nil."
  (should (null (infrasonic--ensure-alist-list nil))))

(ert-deftest infrasonic-test-ensure-alist-list-single-alist ()
  "A single alist should be wrapped in a list."
  (let ((result (infrasonic--ensure-alist-list '((a . 1) (b . 2)))))
    (should (equal (length result) 1))
    (should (equal (caar result) '(a . 1)))))

(ert-deftest infrasonic-test-ensure-alist-list-already-list ()
  "A list of alists should be returned as-is."
  (let* ((input (list (list (cons 'a 1)) (list (cons 'b 2))))
         (result (infrasonic--ensure-alist-list input)))
    (should (equal result input))))

;;;; safe-filename

(ert-deftest infrasonic-test-safe-filename-dangerous-chars ()
  "Dangerous characters should be replaced with underscores."
  (should (equal (infrasonic--safe-filename "test/song:this?has*symbols_.mp3")
                 "test_song_this_has_symbols_.mp3")))

(ert-deftest infrasonic-test-safe-filename-clean ()
  "A filename without dangerous characters should be unchanged."
  (should (equal (infrasonic--safe-filename "nice-song.mp3")
                 "nice-song.mp3")))

(ert-deftest infrasonic-test-safe-filename-whitespace ()
  "Newlines and tabs should be replaced, but spaces should be preserved."
  (should (equal (infrasonic--safe-filename "song\nwith\ttabs.mp3")
                 "song_with_tabs.mp3"))
  (should (equal (infrasonic--safe-filename "song with spaces.mp3")
                 "song with spaces.mp3")))

(ert-deftest infrasonic-test-safe-filename-quotes ()
  "Double quotes should be replaced."
  (should (equal (infrasonic--safe-filename "\"quoted\".mp3")
                 "_quoted_.mp3")))

;;;; child

(ert-deftest infrasonic-test-child-hierarchy ()
  "Child should return the correct next level in the hierarchy."
  (should (eq (infrasonic-child :artists) :artist))
  (should (eq (infrasonic-child :artist) :album))
  (should (eq (infrasonic-child :album) :song))
  (should (eq (infrasonic-child :playlist) :song))
  (should (eq (infrasonic-child :genre) :song)))

(ert-deftest infrasonic-test-child-song-is-leaf ()
  "Song should map to song (leaf node)."
  (should (eq (infrasonic-child :song) :song)))

(ert-deftest infrasonic-test-child-unknown-defaults-to-song ()
  "Unknown levels should default to :song."
  (should (eq (infrasonic-child :nonsense) :song))
  (should (eq (infrasonic-child nil) :song)))

;;;; URL building

(ert-deftest infrasonic-test-build-url-basic ()
  "Should build a correct base URL with protocol, host, and endpoint."
  (let* ((client (infrasonic-test--make-client))
         (url (infrasonic--build-url client "ping" nil)))
    (should (equal url "https://music.example.com/rest/ping.view"))))

(ert-deftest infrasonic-test-build-url-http-protocol ()
  "Should respect the client's protocol setting."
  (let* ((client (infrasonic-test--make-client :protocol "http"))
         (url (infrasonic--build-url client "ping" nil)))
    (should (string-prefix-p "http://music.example.com/" url))))

(ert-deftest infrasonic-test-build-url-with-params ()
  "Should append query parameters."
  (let* ((client (infrasonic-test--make-client))
         (url (infrasonic--build-url client "getSong" '(("id" . "tr-1")))))
    (should (string-prefix-p "https://music.example.com/rest/getSong.view?" url))
    (should (string-match-p "id=tr-1" url))))

(ert-deftest infrasonic-test-build-url-multiple-params ()
  "Should include all query parameters."
  (let* ((client (infrasonic-test--make-client))
         (url (infrasonic--build-url client "search3"
                                     '(("query" . "test")
                                       ("songCount" . "50")))))
    (should (string-match-p "query=test" url))
    (should (string-match-p "songCount=50" url))))

(ert-deftest infrasonic-test-build-url-no-trailing-questionmark ()
  "Should not have a trailing ? when there are no params."
  (let* ((client (infrasonic-test--make-client))
         (url (infrasonic--build-url client "ping" nil)))
    (should-not (string-suffix-p "?" url))))

;;;; URL testing

(ert-deftest infrasonic-test-get-stream-url ()
  "Verify URL contains song ID and auth params."
  (let ((client (infrasonic-test--make-client)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_) (list :user "u" :secret "p"))))
      (let ((url (infrasonic-get-stream-url client "song-42")))
        (should (string-prefix-p "https://music.example.com/rest/stream.view?" url))
        (should (string-match-p "id=song-42" url))
        (should (string-match-p "u=u" url))))))

;;;; Response parsing

(ert-deftest infrasonic-test-process-response-ok ()
  "Should return the subsonic-response alist on success."
  (infrasonic-test--with-json-buffer
      "{\"subsonic-response\":{\"status\":\"ok\",\"version\":\"1.16.1\"}}"
    (let ((result (infrasonic--process-api-response)))
      (should (equal (alist-get 'status result) "ok"))
      (should (equal (alist-get 'version result) "1.16.1")))))

(ert-deftest infrasonic-test-process-response-with-data ()
  "Should return nested data within the response."
  (infrasonic-test--with-json-buffer
      "{\"subsonic-response\":{\"status\":\"ok\",\"song\":{\"id\":\"1\",\"title\":\"Test\"}}}"
    (let* ((result (infrasonic--process-api-response))
           (song (alist-get 'song result)))
      (should (equal (alist-get 'id song) "1"))
      (should (equal (alist-get 'title song) "Test")))))

(ert-deftest infrasonic-test-process-response-api-error ()
  "Should signal infrasonic-api-error when status is not ok."
  (should-error
   (infrasonic-test--with-json-buffer
       "{\"subsonic-response\":{\"status\":\"failed\",\"error\":{\"code\":40,\"message\":\"Wrong credentials\"}}}"
     (infrasonic--process-api-response))
   :type 'infrasonic-api-error))

(ert-deftest infrasonic-test-process-response-empty-buffer ()
  "Should signal infrasonic-api-error on empty buffer."
  (should-error
   (infrasonic-test--with-json-buffer ""
     (infrasonic--process-api-response))
   :type 'infrasonic-api-error))

(ert-deftest infrasonic-test-process-response-invalid-json ()
  "Should signal infrasonic-error on malformed JSON."
  (should-error
   (infrasonic-test--with-json-buffer "this is not json"
     (infrasonic--process-api-response))
   :type 'infrasonic-error))

(ert-deftest infrasonic-test-process-response-missing-subsonic-response ()
  "Should signal infrasonic-api-error when subsonic-response key is missing."
  (should-error
   (infrasonic-test--with-json-buffer "{\"something-else\":{}}"
     (infrasonic--process-api-response))
   :type 'infrasonic-api-error))

(ert-deftest infrasonic-test-process-response-null-values ()
  "Should parse null values as nil."
  (infrasonic-test--with-json-buffer
      "{\"subsonic-response\":{\"status\":\"ok\",\"song\":{\"id\":\"1\",\"genre\":null}}}"
    (let* ((result (infrasonic--process-api-response))
           (song (alist-get 'song result)))
      (should (null (alist-get 'genre song))))))

(ert-deftest infrasonic-test-process-response-array ()
  "Should parse arrays as lists."
  (infrasonic-test--with-json-buffer
      "{\"subsonic-response\":{\"status\":\"ok\",\"songs\":[{\"id\":\"1\"},{\"id\":\"2\"}]}}"
    (let* ((result (infrasonic--process-api-response))
           (songs (alist-get 'songs result)))
      (should (listp songs))
      (should (equal (length songs) 2)))))

;;;; Auth params (mocked)

(ert-deftest infrasonic-test-get-auth-params-structure ()
  "Auth params should contain the required keys."
  (let ((client (infrasonic-test--make-client)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_)
                 (list :user "testuser"
                       :secret "testpassword"))))
      (let ((params (infrasonic--get-auth-params client)))
        (should (assoc "u" params))
        (should (assoc "t" params))
        (should (assoc "s" params))
        (should (assoc "v" params))
        (should (assoc "c" params))
        (should (assoc "f" params))))))

(ert-deftest infrasonic-test-get-auth-params-values ()
  "Auth params should contain correct user, version, agent, and format."
  (let ((client (infrasonic-test--make-client
                 :user-agent "test-player"
                 :api-version "1.15.0")))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_)
                 (list :user "alice"
                       :secret "hunter2"))))
      (let ((params (infrasonic--get-auth-params client)))
        (should (equal (alist-get "u" params nil nil #'equal) "alice"))
        (should (equal (alist-get "v" params nil nil #'equal) "1.15.0"))
        (should (equal (alist-get "c" params nil nil #'equal) "test-player"))
        (should (equal (alist-get "f" params nil nil #'equal) "json"))))))

(ert-deftest infrasonic-test-get-auth-params-token-is-md5 ()
  "Token should be a 32-character hex string (MD5 hash)."
  (let ((client (infrasonic-test--make-client)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_)
                 (list :user "testuser"
                       :secret "testpass"))))
      (let* ((params (infrasonic--get-auth-params client))
             (token (alist-get "t" params nil nil #'equal)))
        (should (stringp token))
        (should (equal (length token) 32))
        (should (string-match-p "\\`[0-9a-f]\\{32\\}\\'" token))))))

(ert-deftest infrasonic-test-get-auth-params-salt-varies ()
  "Each call should generate a different salt."
  (let ((client (infrasonic-test--make-client)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_)
                 (list :user "testuser"
                       :secret "testpass"))))
      (let* ((params1 (infrasonic--get-auth-params client))
             (params2 (infrasonic--get-auth-params client))
             (salt1 (alist-get "s" params1 nil nil #'equal))
             (salt2 (alist-get "s" params2 nil nil #'equal)))
        ;; Salts _could_ theoretically collide, but with 24 bits
        ;; the chance is negligible for a single test
        (should-not (equal salt1 salt2))))))

(ert-deftest infrasonic-test-get-auth-params-token-correctness ()
  "Token should equal md5(password + salt)."
  (let ((client (infrasonic-test--make-client)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_)
                 (list :user "testuser"
                       :secret "mypassword"))))
      (let* ((params (infrasonic--get-auth-params client))
             (salt (alist-get "s" params nil nil #'equal))
             (token (alist-get "t" params nil nil #'equal))
             (expected (secure-hash 'md5 (concat "mypassword" salt))))
        (should (equal token expected))))))

(ert-deftest infrasonic-test-get-auth-params-callable-secret ()
  "Auth should work when :secret is a function (as auth-source provides)."
  (let ((client (infrasonic-test--make-client)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_)
                 (list :user "testuser"
                       :secret (lambda () "funcpassword")))))
      (let* ((params (infrasonic--get-auth-params client))
             (salt (alist-get "s" params nil nil #'equal))
             (token (alist-get "t" params nil nil #'equal))
             (expected (secure-hash 'md5 (concat "funcpassword" salt))))
        (should (equal token expected))))))

(ert-deftest infrasonic-test-get-auth-params-no-credentials ()
  "Should signal an error when no credentials are found."
  (let ((client (infrasonic-test--make-client)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_) nil)))
      (should-error (infrasonic--get-auth-params client)
                    :type 'infrasonic-error))))

;;;; Input validation

(ert-deftest infrasonic-test-set-rating-valid ()
  "Ratings 0 through 5 should not signal a validation error."
  (dolist (rating '(0 1 2 3 4 5))
    ;; Mock the API call so we never hit the network.
    ;; If validation fails, infrasonic-error is signaled before this mock runs.
    (let ((client (infrasonic-test--make-client)))
      (cl-letf (((symbol-function 'infrasonic-api-call)
                 (lambda (&rest _) nil)))
        (should-not (infrasonic-set-rating client "id" rating))))))

(ert-deftest infrasonic-test-set-rating-invalid-too-high ()
  "Ratings above 5 should signal an error."
  (let ((client (infrasonic-test--make-client)))
    (should-error (infrasonic-set-rating client "id" 6)
                  :type 'infrasonic-error)))

(ert-deftest infrasonic-test-set-rating-invalid-negative ()
  "Negative ratings should signal an error."
  (let ((client (infrasonic-test--make-client)))
    (should-error (infrasonic-set-rating client "id" -1)
                  :type 'infrasonic-error)))

(ert-deftest infrasonic-test-set-rating-invalid-non-integer ()
  "Non-integer ratings should signal an error."
  (let ((client (infrasonic-test--make-client)))
    (should-error (infrasonic-set-rating client "id" 3.5)
                  :type 'infrasonic-error)
    (should-error (infrasonic-set-rating client "id" "3")
                  :type 'infrasonic-error)))

(ert-deftest infrasonic-test-set-rating-sends-correct-params ()
  "Ensure that the right params are actually sent."
  (let (captured-params)
    (cl-letf (((symbol-function 'infrasonic-api-call)
               (lambda (_client _endpoint params &rest _)
                 (setq captured-params params)
                 nil)))
      (let ((client (infrasonic-test--make-client)))
        (infrasonic-set-rating client "song-1" 3)
        (should (equal (alist-get "id" captured-params nil nil #'equal) "song-1"))
        (should (equal (alist-get "rating" captured-params nil nil #'equal) "3"))))))

(ert-deftest infrasonic-test-scrobble-invalid-status ()
  "Scrobble with an invalid status should signal an error."
  (let ((client (infrasonic-test--make-client)))
    (should-error (infrasonic-scrobble client "id" :invalid)
                  :type 'infrasonic-error)))

(ert-deftest infrasonic-test-api-call-rejects-non-client ()
  "Passing a non-client struct to api-call should signal a type error."
  (should-error (infrasonic-api-call '(:url "example.com") "ping")
                :type 'wrong-type-argument))

;;;; Playlists

(ert-deftest infrasonic-test-get-playlists-shape ()
  "Ensure that playlists have consistent shape of ((name . id) ...)."
  (cl-letf (((symbol-function 'infrasonic-api-call)
             (lambda (&rest _)
               '((playlists (playlist ((id . "1") (name . "Chill"))
                                     ((id . "2") (name . "Rock"))))))))
    (let* ((client (infrasonic-test--make-client))
           (result (infrasonic-get-playlists client)))
      (should (equal (car result) '("Chill" . "1")))
      (should (equal (length result) 2)))))

;;;; Art URL

(ert-deftest infrasonic-test-get-art-url-default-size ()
  "Art URL should use the client's default art-size."
  (let ((client (infrasonic-test--make-client :art-size 256)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_) (list :user "u" :secret "p"))))
      (let ((url (infrasonic--get-art-url client "al-1")))
        (should (string-prefix-p "https://music.example.com/rest/getCoverArt.view?" url))
        (should (string-match-p "id=al-1" url))
        (should (string-match-p "size=256" url))))))

(ert-deftest infrasonic-test-get-art-url-override-size ()
  "Passing a size should override the client default."
  (let ((client (infrasonic-test--make-client :art-size 64)))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_) (list :user "u" :secret "p"))))
      (let ((url (infrasonic--get-art-url client "al-1" 512)))
        (should (string-match-p "size=512" url))
        (should-not (string-match-p "size=64" url))))))

;;;; Scrobble status mapping

(ert-deftest infrasonic-test-scrobble-playing-sends-false ()
  "Scrobbling :playing should send submission=false."
  (let (captured-params)
    (cl-letf (((symbol-function 'infrasonic-api-call)
               (lambda (_client _endpoint params &rest _)
                 (setq captured-params params)
                 nil)))
      (let ((client (infrasonic-test--make-client)))
        (infrasonic-scrobble client "song-1" :playing)
        (should (equal (alist-get "submission" captured-params nil nil #'equal)
                       "false"))
        (should (equal (alist-get "id" captured-params nil nil #'equal)
                       "song-1"))))))

(ert-deftest infrasonic-test-scrobble-finished-sends-true ()
  "Scrobbling :finished should send submission=true."
  (let (captured-params)
    (cl-letf (((symbol-function 'infrasonic-api-call)
               (lambda (_client _endpoint params &rest _)
                 (setq captured-params params)
                 nil)))
      (let ((client (infrasonic-test--make-client)))
        (infrasonic-scrobble client "song-1" :finished)
        (should (equal (alist-get "submission" captured-params nil nil #'equal)
                       "true"))))))

;;;; Star endpoint selection

(ert-deftest infrasonic-test-star-uses-star-endpoint ()
  "Starring an item should call the \"star\" endpoint."
  (let (captured-endpoint)
    (cl-letf (((symbol-function 'infrasonic-api-call)
               (lambda (_client endpoint &rest _)
                 (setq captured-endpoint endpoint)
                 nil)))
      (let ((client (infrasonic-test--make-client)))
        (infrasonic-star client "id-1" t)
        (should (equal captured-endpoint "star"))))))

(ert-deftest infrasonic-test-star-uses-unstar-endpoint ()
  "Unstarring an item should call the \"unstar\" endpoint."
  (let (captured-endpoint)
    (cl-letf (((symbol-function 'infrasonic-api-call)
               (lambda (_client endpoint &rest _)
                 (setq captured-endpoint endpoint)
                 nil)))
      (let ((client (infrasonic-test--make-client)))
        (infrasonic-star client "id-1" nil)
        (should (equal captured-endpoint "unstar"))))))

;;;; Credential caching

(ert-deftest infrasonic-test-auth-caches-credentials ()
  "Auth params should cache credentials after first lookup."
  (let ((client (infrasonic-test--make-client))
        (call-count 0))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_)
                 (cl-incf call-count)
                 (list :user "u" :secret "p"))))
      ;; First call: cache miss, hits get-credentials
      (infrasonic--get-auth-params client)
      (should (equal call-count 1))
      ;; Second call: cache hit, should NOT call get-credentials again
      (infrasonic--get-auth-params client)
      (should (equal call-count 1)))))

(ert-deftest infrasonic-test-clear-credentials ()
  "Clearing credentials should force a fresh lookup on next call."
  (let ((client (infrasonic-test--make-client))
        (call-count 0))
    (cl-letf (((symbol-function 'infrasonic--get-credentials)
               (lambda (_)
                 (cl-incf call-count)
                 (list :user "u" :secret "p"))))
      (infrasonic--get-auth-params client)
      (should (equal call-count 1))
      (infrasonic-clear-credentials client)
      (should (null (infrasonic-client-cached-credentials client)))
      ;; Next call should hit get-credentials again
      (infrasonic--get-auth-params client)
      (should (equal call-count 2)))))

;;;; Standardise in-place mutation

(ert-deftest infrasonic-test-standardise-eq-with-nconc ()
  "Standardise should now return the same cons cell (eq) via nconc."
  (let* ((item (list (cons 'name "Song") (cons 'id "1")))
         (result (infrasonic--standardise item :song)))
    (should (eq item result))
    (should (eq (alist-get 'subsonic-type result) :song))
    (should (equal (alist-get 'name result) "Song"))))

(ert-deftest infrasonic-test-standardise-eq-when-name-missing ()
  "Standardise should still be eq even when name must be added."
  (let* ((item (list (cons 'title "Song") (cons 'id "1")))
         (result (infrasonic--standardise item :song)))
    (should (eq item result))
    (should (equal (alist-get 'name result) "Song"))))

;;;; Update playlist with nil song-ids

(ert-deftest infrasonic-test-update-playlist-nil-songs-no-songid ()
  "Passing nil for song-ids should not include any songId in the body."
  (let (captured-body)
    (cl-letf (((symbol-function 'infrasonic-api-call)
               (lambda (_client _endpoint _params _callback _errback body &rest _)
                 (setq captured-body body)
                 nil)))
      (let ((client (infrasonic-test--make-client)))
        (infrasonic-update-playlist client "pl-1" nil "New Name")
        (should (stringp captured-body))
        (should (string-match-p "id=pl-1" captured-body))
        (should (string-match-p "name=New" captured-body))
        (should-not (string-match-p "songId" captured-body))))))

(ert-deftest infrasonic-test-update-playlist-with-songs-includes-songid ()
  "Passing song-ids should include songId params in the body."
  (let (captured-body)
    (cl-letf (((symbol-function 'infrasonic-api-call)
               (lambda (_client _endpoint _params _callback _errback body &rest _)
                 (setq captured-body body)
                 nil)))
      (let ((client (infrasonic-test--make-client)))
        (infrasonic-update-playlist client "pl-1" '("s1" "s2") "My Playlist")
        (should (string-match-p "songId=s1" captured-body))
        (should (string-match-p "songId=s2" captured-body))))))

(provide 'infrasonic-test)

;;; infrasonic-test.el ends here
