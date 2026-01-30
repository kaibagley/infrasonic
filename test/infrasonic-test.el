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

;;; Code:

(require 'ert)
(require 'plz)
(require 'infrasonic)

;; Client Tests

(ert-deftest test-make-client ()
  "Can we create a client?"
  (let ((client (infrasonic-make-client :url "music.example.com")))
    (should (equal (infrasonic--client-get client :url) "music.example.com"))))

(ert-deftest test-validate-client-valid ()
  "Can we properly validate a valid client?"
  (let ((client (infrasonic-make-client :url "music.example.com")))
    (should-not (infrasonic--validate-client client))))

(ert-deftest test-validate-client-missing-url ()
  "Can we detect an invalid client?"
  (let ((client (infrasonic-make-client)))
    (should-error (infrasonic--validate-client client) :type 'infrasonic-error)))

(ert-deftest test-validate-client-invalid-protocol ()
  "Can we detect an invalid client due to bad protocol?"
  (let ((client (infrasonic-make-client :url "music.example.com" :protocol "ftp")))
    (should-error (infrasonic--validate-client client) :type 'infrasonic-error)))

;; Standardisation Tests

(ert-deftest test-standardise-song ()
  "Can we standardise a song?"
  (let ((song (list (cons 'title "Test Song")
                    (cons 'artist "Test Artist")
                    (cons 'id "tr-1"))))
    (let ((standardised (infrasonic--standardise song :song)))
      (should (eq (alist-get 'subsonic-type standardised) :song))
      (should (equal (alist-get 'name standardised) "Test Song")))))

(ert-deftest test-standardise-album ()
  "Can we standardise an album?"
  (let ((album (list (cons 'name "Test Album")
                    (cons 'artist "Test Artist")
                    (cons 'id "al-1"))))
    (let ((standardised (infrasonic--standardise album :album)))
      (should (eq (alist-get 'subsonic-type standardised) :album))
      (should (equal (alist-get 'name standardised) "Test Album")))))

(ert-deftest test-standardise-artist ()
  "Can we standardise an artist?"
  (let ((artist (list (cons 'name "Test Artist")
                     (cons 'id "ar-1"))))
    (let ((standardised (infrasonic--standardise artist :artist)))
      (should (eq (alist-get 'subsonic-type standardised) :artist))
      (should (equal (alist-get 'name standardised) "Test Artist")))))

(ert-deftest test-standardise-list ()
  "Can we standardise a list of items?"
  (let ((item1 (list (cons 'name "Item 1") (cons 'id "1")))
        (item2 (list (cons 'name "Item 2") (cons 'id "2")))
        (items (list item1 item2)))
    (let ((standardised (infrasonic--standardise-list items :song)))
      (should (equal (length standardised) 2))
      (should (eq (alist-get 'subsonic-type (car standardised)) :song)))))

;; Helper Function Tests

(ert-deftest test-child-levels ()
  "Is the hierarchy in order?"
  (should (equal (infrasonic-child :artists) :artist))
  (should (equal (infrasonic-child :artist) :album))
  (should (equal (infrasonic-child :album) :song))
  (should (equal (infrasonic-child :song) :song))
  (should (equal (infrasonic-child :playlist) :song))
  (should (equal (infrasonic-child :genre) :song)))

(ert-deftest test-safe-filename ()
  "Is the safe filename function safe?"
  (should (equal (infrasonic--safe-filename "test/song:this?has*symbols_.mp3")
                 "test_song_this_has_symbols_.mp3")))

(ert-deftest test-alist-p ()
  "Do we detect alists?"
  (should (infrasonic--alist-p (list (cons 'a 1) (cons 'b 2))))
  (should-not (infrasonic--alist-p nil))
  (should-not (infrasonic--alist-p '(a b c))))

(ert-deftest test-ensure-alist-list ()
  "Can we convert objects to lists of alists?"
  (should (equal (infrasonic--ensure-alist-list nil) nil))
  (should (equal (infrasonic--ensure-alist-list (list (cons 'a 1)))
                 (list (list (cons 'a 1)))))
  (should (equal (infrasonic--ensure-alist-list (list (cons 'a 1) (cons 'b 2)))
                 (list (cons 'a 1) (cons 'b 2)))))

(provide 'infrasonic-test)

;;; infrasonic-test.el ends here
