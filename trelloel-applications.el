;;; trelloel-applications.el --- Communication layer for the Trello API

;; Copyright (C) 2012 Jeremiah Dodds

;; Author: Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Keywords: comm

;; This file is not a part of GNU Emacs.

;; trelloel-applications.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; trelloel-applications.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with trelloel-applications.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provide functions for dealing with client applications

;;; Code:

(require 'trelloel-oauth)

(defun trelloel-applications--library (application)
  (expand-file-name
   (concat user-emacs-directory application "-trelloel.el")))

(defun trelloel-applications-register (application &optional force)
  (unless (file-exists-p (trelloel-oauth--token-file application))
    (trelloel-oauth--set-token application))
  (trelloel-applications--initialize application force))

(setq trelloel-applications--functions
      '(("get-board" . (id))
        ("get-members-boards" . ())
        ("get-boards-cards" . (board))
        ("get-boards-lists" . (board))
        ("create-card-on-list" . (name description list-id))
        ("delete-card" . (card-id))))

(defmacro trelloel-applications--create-alias (app f args )
  (let ((creating-sym (intern (concat app "-" f)))
        (calling-sym (intern (concat "trelloel-api--" f))))
   `(defun ,creating-sym ,args
      (,calling-sym ,app ,@args))))

(defun trelloel-applications--write-app (application)
  (with-temp-buffer
    (dolist (info trelloel-applications--functions)
      (insert
       (prin1-to-string
        (macroexpand
         `(trelloel-applications--create-alias
           ,application
           ,(car info)
           ,(cdr info))))))
    (write-region
     (point-min) (point-max)
     (trelloel-applications--library application))))

(defun trelloel-applications--initialize (application &optional force)
  (when (or force
            (not (file-exists-p (trelloel-applications--library application))))
    (trelloel-applications--write-app application))
  (load-file (trelloel-applications--library application)))

(provide 'trelloel-applications)
;;; trellol-applications.el ends here
