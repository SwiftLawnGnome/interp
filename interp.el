;;; interp.el --- String interpolation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Zach Shaftel
;;
;; Author: Zach Shaftel <http://github/zach>
;; Maintainer: Zach Shaftel <zshaftel@gmail.com>
;; Created: April 17, 2020
;; Modified: April 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/zach/interp
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  String interpolation
;;
;;; Code:

(require 'dash)
(eval-when-compile
  (require 'rx)
  (require 'tco (expand-file-name "../tco/tco.el")))

(defvar interp-expr-regex
  (rx "$"
      (? (group-n 2 "@"))
      "{"
      (group-n 1 (+ (not "}")))
      "}"))

(defun interp--substring-or-nil (s start end)
  (unless (eq start end)
    (substring s start end)))

(defun interp--match (string num)
  (interp--substring-or-nil
   string
   (match-beginning num)
   (match-end num)))

(defun interp--collect (str)
  (save-match-data
    (tco-loop ((start 0)
               (strings-exprs nil))
      (-if-let (ss (string-match interp-expr-regex str start))
          (let* ((end (match-end 0))
                 (beg (match-beginning 0))
                 (prev (interp--substring-or-nil str start beg))
                 (splice? (match-beginning 2)))
            (tco-recur
             end
             `((,(if splice? :splice :expr)
                 ,(interp--match str 1))
               ,prev
               ,@strings-exprs)))
        (unless (eq start (length str))
          (push (substring str start (length str)) strings-exprs))
        (nreverse strings-exprs)))))

(defun interp--convert (string)
  (-let* ((c (interp--collect string))
          fmtparts fmtargs)
    (dolist (part c)
      (pcase part
        ((pred stringp)
         (push (replace-regexp-in-string "%" "%%" part) fmtparts))
        (`(:expr ,expr)
          (push "%s" fmtparts)
          (push (read expr) fmtargs))
        (`(:splice ,expr)
          (push "%s" fmtparts)
          (push `(concat ,(read expr)) fmtargs))))
    `(format ,(apply #'concat (nreverse fmtparts)) ,@(nreverse fmtargs))))

(defmacro interp (string)
  (interp--convert string))

(provide 'interp)
;;; interp.el ends here
