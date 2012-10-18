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


(defgroup trelloel nil
  "settings for working with the trello API")

(defcustom trelloel-username nil
  "Your username on trello."
  :type 'string
  :group 'trelloel)

(defgroup trelloel-authorization nil
  "settings related to authorizing with trello"
  :group 'trelloel)

(defcustom trelloel-oauth-token nil
  "Token used for OAuth authentication"
  :type 'string
  :group 'trelloel-authorization)

(defun trelloel-get-board (id)
  (let* ((board-url (concat trelloel--base-api-url
                            "/" trelloel--api-version
                            "/board/" id
                           "?key=" trelloel--application-key))
         (json-object-type 'plist)
         (board nil)
         (board-buffer (url-retrieve-synchronously board-url)))
    (save-excursion
      (set-buffer board-buffer)
      (goto-char (point-min))
      (search-forward "{")
      (backward-char)
      (setq board (json-read))
      (kill-buffer (current-buffer)))
    board))

(defun trelloel--get-oauth-token (app-name)
  (unless trelloel-oauth-token
    (let ((auth-url
           (concat "https://trello.com/1/authorize?"
                   "key=" trelloel--application-key
                   "&name=" (url-hexify-string app-name)
                   "&expiration=never"
                   "&response_type=token"
                   "&scope=read,write")))
      (browse-url auth-url))
    (let ((token (read-from-minibuffer "Token: ")))
      (custom-set-variables
       `(trelloel-oauth-token ,token))))
  trelloel-oauth-token)

(defun trelloel-get-users-boards (app-name)
  (let* ((oauth-token (trelloel--get-oauth-token app-name))
         (request-url (concat trelloel--base-api-url
                              "/" trelloel--api-version
                              "/members/my/boards"
                              "?key=" trelloel--application-key
                              "&token=" oauth-token))
         (json-object-type 'plist)
         (boards nil)
         (boards-buffer (url-retrieve-synchronously request-url)))
    (save-excursion
      (set-buffer boards-buffer)
      (goto-char (point-min))
      (search-forward "{")
      (backward-char)
      (setq boards (json-read))
      (kill-buffer (current-buffer)))
    boards))

(provide 'trelloel)
;;; trellol.el ends here
