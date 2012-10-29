;;; trelloel-api.el --- Communication layer for the Trello API

;; Copyright (C) 2012 Jeremiah Dodds

;; Author: Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Keywords: comm

;; This file is not a part of GNU Emacs.

;; trelloel-api.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; trelloel-api.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with trelloel-api.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides access to the Trello api.

;;; Code:

(defun trelloel-api--read-json-buffer (json-buffer)
  (let ((parsed-json nil))
    (save-excursion
      (set-buffer json-buffer)
      (goto-char (point-min))
      (search-forward "{")
      (backward-char)
      (setq parsed-json (json-read))
      (kill-buffer (current-buffer)))
    parsed-json))

(defun trelloel-api--request-parameters (application &optional parts)
  (let ((parameters (concat "?key=" trelloel--application-key
                            "&name=" application)))
    (if parts
        (concat parameters (mapconcat
                            (lambda (item)
                              (concat "&" (car item) "=" (cdr item)))
                            parts ""))
      parameters)))

(defun trelloel-api--request-url (application section &optional parts)
  (let ((request-parameters (trelloel-api--request-parameters application parts)))
    (concat trelloel--base-api-url
            "/" trelloel--api-version
            section
            request-parameters)))

(defun trelloel-api--authorized-request-url (application section &optional parts)
  (let* ((oauth-token (trelloel-oauth--get-token application)))
    (trelloel-api--request-url
     application section
     (append `(("token" . ,oauth-token)) parts))))


(defun trelloel-api--api-result (api-url)
  (trelloel-api--read-json-buffer (url-retrieve-synchronously api-url)))

(defun trelloel-api--authorized-api-result (application section &optional parts)
  (let ((request-url (trelloel-api--authorized-request-url
                      application section
                      parts))
        (json-object-type 'plist))
    (trelloel-api--api-result request-url)))

(defun trelloel-api--get-board (application id)
  (trelloel-api--authorized-api-result
   application
   (concat "/board/" id)))

(defun trelloel-api--get-members-boards (application)
  (trelloel-api--authorized-api-result
   application
   "/members/my/boards"
   '(("filter" . "open"))))

(defun trelloel-api--get-boards-cards (application board)
  (trelloel-api--authorized-api-result
   application
   (concat "/boards/" board "/cards")))

(defun trelloel-api--get-boards-lists (application board)
  (trelloel-api--authorized-api-result
   application
   (concat "/boards/" board "/lists")))

(provide 'trelloel-api)

;;; trelloel-api.el ends here
