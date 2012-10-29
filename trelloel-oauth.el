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


(defun trelloel-oauth--get-token (application)
  (trelloel-oauth--load-token application))

(defun trelloel-oauth--token-file (application)
  (expand-file-name
   (concat user-emacs-directory application "-oauth-token")))

(defun trelloel-oauth--load-token (application)
  (let ((oauth-token-file (trelloel-oauth--token-file application)))
    (unless (file-exists-p oauth-token-file)
      (trelloel-oauth--set-token application))
    (trelloel-oauth--read-token application)))

(defun trelloel-oauth--set-token (application)
  (let ((auth-url
         (concat "https://trello.com/1/authorize"
                 (trelloel--request-parameters
                  application
                  `(("expiration" . "never")
                    ("response_type" . "token")
                    ("scope" . "read,write"))))))
    (browse-url auth-url))
  (let ((token (read-from-minibuffer "Token: ")))
    (trelloel-oauth--save-token application token)))

(defun trelloel-oauth--save-token (application token)
  (with-temp-buffer
    (insert token)
    (write-region
     (point-min) (point-max)
     (trelloel-oauth--token-file application))))

(defun trelloel-oauth--read-token (application)
  (with-temp-buffer
    (insert-file-contents (trelloel-oauth--token-file application))
    (buffer-string)))

(provide 'trelloel-oauth)

;;; trelloel-oauth.el ends here
