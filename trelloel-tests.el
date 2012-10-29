(trelloel-applications-register "trelloel-tests")

(setq trelloel-test-board-id "508632c0a7d1eb475400203b")

(ert-deftest trelloel-get-board ()
  (let ((board (trelloel-tests-get-board trelloel-test-board-id)))
    (should (equal "Trelloel Test Board" (plist-get board :name))))
  (let ((boards (trelloel-tests-get-members-boards)))
    (should (not (null boards)))))

(ert-deftest trelloel-get-boards-cards ()
  (let ((cards (trelloel-tests-get-boards-cards trelloel-test-board-id)))
    (should (not (null cards)))))

(ert-deftest trelloel-get-boards-lists ()
  (let ((lists (trelloel-tests-get-boards-lists trelloel-test-board-id)))
    (should (not (null lists)))))
