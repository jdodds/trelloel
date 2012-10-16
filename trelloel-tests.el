(ert-deftest trelloel-get-board ()
  (let ((board (trelloel-get-board "4d5ea62fd76aa1136000000c")))
    (should (equal "Trello Development" (plist-get board :name))))
  (should (not (null (trelloel-get-users-boards "trelloel-tests")))))

(ert-deftest trelloel-auth ()
  (should (stringp (trelloel--get-oauth-token "trelloel-tests"))))
