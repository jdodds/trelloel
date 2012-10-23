;;; trelloel-oauth.el --- Oauth functions for trelloel

;; Copyright (C) 2012 Jeremiah Dodds

;; Author: Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Keywords: comm

;; This file is not a part of GNU Emacs.

;; trelloel-oauth.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; trelloel-oauth.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with trelloel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides auxillary functions for authenticating using oauth for
;; the trelloel library.

;;; Code:

(defvar trelloel--oauth-token nil "token used in oauth authentication")

(defvar trelloel--oauth-token-file
  (expand-file-name
   (concat user-emacs-directory "trelloel-oauth-token")))

(defun trelloel--get-oauth-token ()
  (unless trelloel--oauth-token
    (trelloel--load-oauth-token))
  (trelloel--read-oauth-token))

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

(provide 'trelloel-oauth)

;;; trelloel-oauth.el ends here
