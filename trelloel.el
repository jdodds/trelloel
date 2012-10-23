;;; trelloel.el --- Communication layer for the Trello API

;; Copyright (C) 2012 Jeremiah Dodds

;; Author: Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Keywords: comm

;; This file is not a part of GNU Emacs.

;; trelloel.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; trelloel.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with trelloel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides access to the Trello api.

;;; Code:

(require 'json)
(require 'trelloel-oauth)

(setq trelloel--application-key "dd89b2a1fb793c17fb2b72f01a4d3565"
      trelloel--base-api-url "https://api.trello.com"
      trelloel--api-version "1")

(defgroup trelloel nil
  "settings for working with the trello API")

(defcustom trelloel-username nil
  "Your username on trello."
  :type 'string
  :group 'trelloel)

(defun trelloel--read-json-buffer (json-buffer)
  (let ((parsed-json nil))
    (save-excursion
      (set-buffer json-buffer)
      (goto-char (point-min))
      (search-forward "{")
      (backward-char)
      (setq parsed-json (json-read))
      (kill-buffer (current-buffer)))
    parsed-json))

(defun trelloel--request-parameters (&optional parts)
  (let ((parameters (concat "?key=" trelloel--application-key)))
    (if parts
        (concat parameters (mapconcat
                            (lambda (item)
                              (concat "&" (car item) "=" (cdr item)))
                            parts ""))
      parameters)))

(defun trelloel--request-url (section &optional parts)
  (let ((request-parameters (trelloel--request-parameters parts)))
    (concat trelloel--base-api-url
          "/" trelloel--api-version
          section
          request-parameters)))

(defun trelloel--authorized-request-url (section &optional parts)
  (let* ((oauth-token (trelloel--get-oauth-token)))
    (trelloel--request-url
     section
     (append `(("token" . ,oauth-token)) parts))))


(defun trelloel--api-result (api-url)
  (trelloel--read-json-buffer (url-retrieve-synchronously api-url)))

(defun trelloel-get-board (id)
  (let* ((board-url (trelloel--request-url (concat "/board/" id)))
         (json-object-type 'plist))
    (trelloel--api-result board-url)))

(defun trelloel--authorized-api-result (section &optional parts)
  (let ((request-url (trelloel--authorized-request-url
                      section
                      parts))
        (json-object-type 'plist))
    (trelloel--api-result request-url)))

(defun trelloel-get-members-boards ()
  (trelloel--authorized-api-result
   "/members/my/boards"
   '(("filter" . "open"))))

(defun trelloel-get-boards-cards (board)
  (trelloel--authorized-api-result
   (concat "/boards/" board "/cards")))

(provide 'trelloel)

;;; trellol.el ends here
