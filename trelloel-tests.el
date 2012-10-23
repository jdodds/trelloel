(setq trelloel-test-board-id "508632c0a7d1eb475400203b")

(ert-deftest trelloel-get-board ()
  (let ((board (trelloel-get-board trelloel-test-board-id)))
    (should (equal "Trelloel Test Board" (plist-get board :name))))
  (let ((boards (trelloel-get-members-boards)))
    (should (not (null boards)))))

(ert-deftest trelloel-get-boards-cards ()
  (let ((cards (trelloel-get-boards-cards trelloel-test-board-id)))
    (should (not (null cards)))))

(ert-deftest trelloel-auth ()
  (should (stringp (trelloel--get-oauth-token))))

(ert-deftest trelloel-url-creation ()
  (let ((key-parameter (concat "?key=" trelloel--application-key)))
    (should (equal
             (concat key-parameter "&foo=bar&baz=quux")
             (trelloel--request-parameters
              '(("foo" . "bar")
                ("baz" . "quux")))))
    (should (equal key-parameter (trelloel--request-parameters)))))
