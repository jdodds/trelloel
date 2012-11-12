(trelloel-applications-register "trelloel-tests" t)

(setq trelloel-tests-board "508632c0a7d1eb475400203b"
      trelloel-tests-todo "508632c0a7d1eb475400203c"
      trelloel-tests-doing "508632c0a7d1eb475400203d"
      trelloel-tests-done "508632c0a7d1eb475400203e")


(ert-deftest trelloel-get-board ()
  (let ((board (trelloel-tests-get-board trelloel-tests-board)))
    (should (equal "Trelloel Test Board" (plist-get board :name))))
  (let ((boards (trelloel-tests-get-members-boards)))
    (should (not (null boards)))))

(ert-deftest trelloel-get-boards-cards ()
  (let ((cards (trelloel-tests-get-boards-cards trelloel-tests-board)))
    (should (not (null cards)))))

(ert-deftest trelloel-get-boards-lists ()
  (let ((lists (trelloel-tests-get-boards-lists trelloel-tests-board)))
    (should (not (null lists)))))


(ert-deftest trelloel-create-delete-card ()
  (let ((card (trelloel-tests-create-card-on-list
               "A Card Being Created"
               "With some description text"
                trelloel-tests-doing)))
    (should (equal "A Card Being Created" (plist-get card :name)))
    (should (equal "With some description text" (plist-get card :desc)))
    (trelloel-tests-delete-card (plist-get card :id))))


(provide 'trelloel-tests)
