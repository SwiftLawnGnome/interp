;;; interp-tests.el --- Tests for interp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Zach Shaftel
;;
;; Author: Zach Shaftel <http://github/SwiftLawnGnome>
;; Maintainer: Zach Shaftel <zshaftel@gmail.com>
;; Created: April 23, 2020
;; Modified: April 23, 2020
;; Version: 0.0.1
;; Keywords:
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'ert)
(require 'interp (expand-file-name "interp"))

;; TODO
(ert-deftest interp ()
  (should (equal (interp "$@{\"\"}") ""))
  (should (equal (interp "$%S{\"\"}") "\"\""))
  (should (equal (interp "$%d{?a}") (format "%d" ?a)))
  (should (equal (interp "$%.2f{float-pi}") "3.14"))
  (should (equal (interp "$%c@ {(interp \"$^{(car (list 'hello))}\")}") "H E L L O"))
  (should (equal (interp "$_^{\"Hey Carl, you have GOT to meet мой друг Владимир Ленин!\"}")
                 "hEY cARL, YOU HAVE got TO MEET МОЙ ДРУГ вЛАДИМИР лЕНИН!")))


(provide 'interp-tests)
;;; interp-tests.el ends here
