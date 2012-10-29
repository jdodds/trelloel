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
(require 'trelloel-applications)
(require 'trelloel-api)

(setq trelloel--application-key "dd89b2a1fb793c17fb2b72f01a4d3565"
      trelloel--base-api-url "https://api.trello.com"
      trelloel--api-version "1")

(defgroup trelloel nil
  "settings for working with the trello API")

(defcustom trelloel-username "not set"
  "Your username on trello."
  :type 'string
  :group 'trelloel)


(provide 'trelloel)

;;; trelloel.el ends here
