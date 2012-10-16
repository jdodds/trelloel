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
