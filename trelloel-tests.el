(ert-deftest trelloel-get-board ()
  (let ((board (trelloel-get-board "4d5ea62fd76aa1136000000c")))
    (should (equal "Trello Development" (plist-get board :name))))
  (should (not (null (trelloel-get-users-boards)))))

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
