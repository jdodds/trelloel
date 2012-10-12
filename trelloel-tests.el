(ert-deftest trelloel-get-board ()
  (should (hash-table-p (trelloel-get-board "4d5ea62fd76aa1136000000c"))))
