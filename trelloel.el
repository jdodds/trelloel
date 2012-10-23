;;; trelloel.el --- Communication layer for the Trello API

;; Copyright (C) 2012 Jeremiah Dodds

;; Author: Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Keywords: comm
;; Package-Requires: ((json "1.2"))
;; Version: 0.1alpha

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

(setq trelloel--application-key "dd89b2a1fb793c17fb2b72f01a4d3565"
      trelloel--base-api-url "https://api.trello.com"
      trelloel--api-version "1")

(defvar trelloel--oauth-token nil "token used in oauth authentication")

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

(defun trelloel--api-result (api-url)
  (trelloel--read-json-buffer (url-retrieve-synchronously api-url)))

(defun trelloel--get-oauth-token ()
  (unless trelloel--oauth-token
    (trelloel--load-oauth-token))
  (trelloel--read-oauth-token))

(defvar trelloel--oauth-token-file
  (expand-file-name
   (concat user-emacs-directory "trelloel-oauth-token")))

(defun trelloel--load-oauth-token ()
  (unless (file-exists-p trelloel--oauth-token-file)
    (trelloel--set-oauth-token))
  (trelloel--read-oauth-token))

(defun trelloel--set-oauth-token ()
  (let ((auth-url
         (concat "https://trello.com/1/authorize"
                 (trelloel--request-parameters
                  '(("expiration" . "never")
                    ("response_type" . "token")
                    ("scope" . "read,write"))))))
    (browse-url auth-url))
  (let ((token (read-from-minibuffer "Token: ")))
    (trelloel--save-oauth-token token)))

(defun trelloel--save-oauth-token (token)
  (with-temp-buffer
    (insert token)
    (write-region (point-min) (point-max) trelloel--oauth-token-file)))

(defun trelloel--read-oauth-token ()
  (with-temp-buffer
    (insert-file-contents trelloel--oauth-token-file)
    (buffer-string)))


(defun trelloel-get-board (id)
  (let* ((board-url (trelloel--request-url (concat "/board/" id)))
         (json-object-type 'plist))
    (trelloel--api-result board-url)))

(defun trelloel-get-members-boards ()
  (let* ((oauth-token (trelloel--get-oauth-token))
         (request-url (trelloel--request-url
                       "/members/my/boards"
                       `(("token" . ,oauth-token)
                         ("filter" . "open"))))
         (json-object-type 'plist))
    (trelloel--api-result request-url)))

(defun trelloel-get-boards-cards (board)
  (let* ((oauth-token (trelloel--get-oauth-token))
         (request-url (trelloel--request-url
                       (concat "/boards/" board "/cards")
                       `(("token" . ,oauth-token)))))
    (trelloel--api-result request-url)))


(provide 'trelloel)

;;; trellol.el ends here
