(setq trelloel--application-key "dd89b2a1fb793c17fb2b72f01a4d3565"
      trelloel--base-api-url "https://api.trello.com"
      trelloel--api-version "1")

(defun trelloel-get-board (id)
  (let* ((board-url (concat trelloel--base-api-url
                            "/" trelloel--api-version
                            "/board/" id
                           "?key=" trelloel--application-key))
         (json-object-type 'hash-table)
         (board-buffer (url-retrieve-synchronously board-url)))
    (save-excursion
      (set-buffer board-buffer)
      (message (buffer-string))
      (goto-char (point-min))
      (search-forward "{")
      (backward-char)
      (json-read))))
