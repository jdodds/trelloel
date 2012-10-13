(require 'json)

(setq trelloel--application-key "dd89b2a1fb793c17fb2b72f01a4d3565"
      trelloel--base-api-url "https://api.trello.com"
      trelloel--api-version "1"
      trelloel--oauth-secret
      "4f407114d66909a5a16f3d984f4b7197f491fa5e21e458c45d0c57f64cba3057"
      trelloel--oauth-token nil)

(defgroup trelloel nil
  "settings for working with the trello API")

(defcustom trelloel-username nil
  "Your username on trello."
  :group 'trelloel)


(defun trelloel-get-board (id)
  (let* ((board-url (concat trelloel--base-api-url
                            "/" trelloel--api-version
                            "/board/" id
                           "?key=" trelloel--application-key))
         (json-object-type 'plist)
         (board-buffer (url-retrieve-synchronously board-url)))
    (save-excursion
      (set-buffer board-buffer)
      (goto-char (point-min))
      (search-forward "{")
      (backward-char)
      (json-read))))

(defun trelloel--get-oauth-token (app-name)
  (unless trelloel--oauth-token
    (let* ((auth-url
            (concat "https://trello.com/1/authorize?"
                    "key=" trelloel--application-key
                    "&name=" (url-hexify-string app-name)
                    "&expiration=never"
                    "&response_type=token"
                    "&scope=read,write")))
      (browse-url auth-url))
    (setq trelloel--oauth-token (read-from-minibuffer "Token: ")))
  trelloel--oauth-token)
