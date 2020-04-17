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
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  String interpolation
;;
;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'pcase)
  (require 'inline)
  (require 'tco (expand-file-name "../tco/tco.el")))

(defvar interp--group-rxs
  '((2 "^")
    (3 "_")
    (4 "'")
    (5 "@" (*? anything))))

(defvar interp--expr-regex
  (rx-to-string
   `(and "$"
         ,@(mapcar (lambda (x)
                     `(? (group-n ,@x)))
                   interp--group-rxs)
         "{" (group-n 1 (+ (not "}"))) "}")))

(defun interp--substring-or-nil (s start end)
  (unless (eq start end)
    (substring s start end)))

(defun interp--match (string num)
  (interp--substring-or-nil
   string
   (match-beginning num)
   (match-end num)))

(defmacro interp--let-matches (string &rest body)
  (declare (indent 1))
  (let ((s (gensym "string")))
    `(let* ((,s ,string)
            (begin (match-beginning 0))
            (end (match-end 0))
            (splice? (match-beginning 5))
            (splice-sep
             (let ((spliceend (match-end 5)))
               (and spliceend
                    (not (eq spliceend (1+ splice?)))
                    (substring ,s (1+ splice?) spliceend))))

            (case (cond
                    ((match-beginning 2)
                     (if (match-beginning 3)
                         'capitalize
                       'downcase))
                    ((match-beginning 3) 'upcase)))
            (literal? (match-beginning 4))
            (expression
             (read (substring ,s
                              (match-beginning 1)
                              (match-end 1)))))
       ,@body)))

(defsubst interp--seq->sep-string (x sep)
  (mapconcat (lambda (y) (format "%s" y)) x sep))

(define-inline interp--seq->string (x &optional sep)
  (declare (side-effect-free t))
  (if sep
      (if (inline-const-p x)
          (interp--seq->sep-string x sep)
        (inline-quote (interp--seq->sep-string ,x ,sep)))
    (pcase (inline-const-val x)
      ((pred stringp) x)
      ('nil "")
      ((and (pred sequencep) seq)
       (substring (format "%s" seq) 1 -1))
      (wrong (signal 'wrong-type-argument (list 'sequencep wrong))))))

(defun interp--collect (str)
  (let ((len (length str)))
    (if (zerop len)
        (list str)
      (save-match-data
        (tco-loop ((start 0)
                   (strings-exprs nil))
          (if (string-match interp--expr-regex str start)
              (interp--let-matches str
                (if (and (> begin 0)
                         (eq (aref str (1- begin)) ?\\))
                    (tco-recur end
                               `((,(substring str start end)
                                   ,@strings-exprs)))
                  (when splice?
                    (when literal?
                      (error "Can't use both ' and @ in an `interp' expression"))
                    (setq expression `(interp--seq->string ,expression ,splice-sep)))
                  (when case
                    (setq expression `(,case ,expression)))
                  (tco-recur
                   end
                   `((,(if literal? "%S" "%s") . ,expression)
                     ,(substring str start begin)
                     ,@strings-exprs))))
            (unless (eq start (length str))
              (push (substring str start) strings-exprs))
            (nreverse strings-exprs)))))))

(defun interp--convert (string)
  (let* ((c (interp--collect string))
         fmtparts
         letbinds
         fmtargs)
    (dolist (part c)
      (pcase part
        ((pred stringp)
         (push (replace-regexp-in-string "%" "%%" part) fmtparts))
        ((and `(,fmt . ,expr)
              (let s (gensym "interp-exp")))
         (push fmt fmtparts)
         (push s fmtargs)
         (push (list s expr) letbinds))))
    (let ((fmtstring (apply #'concat (nreverse fmtparts))))
      (if fmtargs
          `(let*  (format ,fmtstring ,@(nreverse fmtargs)))
        fmtstring))))

;;;###autoload
(defmacro interp (istring)
  "Return a string form by processing the interpolated ISTRING."
  (interp--convert istring))

(provide 'interp)
;;; interp.el ends here
