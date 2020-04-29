;;; interp.el --- String interpolation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Zach Shaftel
;;
;; Author: Zach Shaftel <http://github/SwiftLawnGnome>
;; Maintainer: Zach Shaftel <zshaftel@gmail.com>
;; Created: April 17, 2020
;; Modified: April 17, 2020
;; Version: 0.0.1
;; Keywords: 
;; Homepage: https://github.com/SwiftLawnGnome/interp
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  String interpolation is like `format', but instead of labeled positions in
;;  the string and arguments to place there, the expressions are placed in the
;;  string itself. So instead of
;;
;;  (format "%d is a huge number!" most-positive-fixnum)
;;
;;  you can write
;;
;;  (interp "$%d{most-positive-fixnum} is a huge number!")
;;
;;  which expands to the same code.
;;
;;  `interp' expressions start with $, followed by one or more modifiers, and an
;;  expression EXPR enclosed in {}, which is evaluated and inserted into the
;;  output string. Modifiers can be:
;;
;;  - `format' % sequences, which can take all the additional arguments except
;;    for <field>. If not supplied, defaults to %s (the `princ' representation).
;;
;;  - A case modifier, where _ means all lowercase, ^ means all uppercase, ^_
;;    means capitalize, and _^ means inverted case.
;;
;;  - A splice modifier, @, which splices the result of EXPR (which must be a
;;    sequence) into the string. Each element of the sequence is formatted
;;    according to the `format' argument. Any characters which follow @ and
;;    precede the { are used as separators for the spliced sequence.
;;
;;; Code:

(require 'dash)
(eval-when-compile
  (require 'rx)
  (require 'pcase)
  (require 'inline)
  (require 'cl-lib))

(defun interp--substring-or (s start end &optional else)
  (declare (pure t) (side-effect-free t))
  (if (eq start end)
      else
    (substring s start end)))

(defun interp--match (string num &optional else)
  (interp--substring-or string (match-beginning num) (match-end num) else))

(defvar interp--output-buffer
  (or (get-buffer " *interp output*")
      (with-current-buffer (get-buffer-create " *interp output*")
        (let ((st (copy-syntax-table emacs-lisp-mode-syntax-table)))
          ;; make {} delimiters in the buffer, so `scan-sexps' can be used to
          ;; extract the sexp from a ${}
          (modify-syntax-entry ?\{ "(}" st)
          (modify-syntax-entry ?\} "){" st)
          (set-syntax-table st))
        (current-buffer)))
  "Buffer used for reading expressions and generating spliced
sequences in `interp' strings.")

(defvar interp--output-buffer-lock
  (make-mutex "`interp--output-buffer' lock")
  "Mutex held when outputting to `interp--output-buffer'.")

(defun interp--protect-output-buffer ()
  "Prevent `interp--output-buffer' from being killed."
  (not (eq (current-buffer) interp--output-buffer)))

(add-hook 'kill-buffer-query-functions #'interp--protect-output-buffer)

(defmacro interp--with-output-buffer (&rest body)
  "Execute BODY in `interp--output-buffer'.
`interp--output-buffer-lock' is held, (point) is saved, and
buffer is narrowed to an empty region. After execution, any
contents written to the buffer are deleted. BODY must not modify
anything outside of the narrowed region."
  (declare (indent 0))
  `(with-mutex interp--output-buffer-lock
     (with-current-buffer interp--output-buffer
       (save-excursion
         (save-restriction
           (narrow-to-region (point) (point))
           (unwind-protect
                (progn ,@body)
             (delete-region (point-min) (point-max))))))))

;; TODO more specific with the errors
;; it's really tough to figure out what exactly went wrong in `interp--expr-end'
(define-error 'interp-parse-error
    "Error parsing `interp' expression")

;; (define-error 'interp-not-1-form
;;     "Exactly one form is allowed in an `interp' ${...} expression")

(define-error 'interp-no-end-in-sight
    "${ in `interp' string lacks a terminating brace")

;; (define-error 'interp-unbalanced-expression
;;     "Expression in `interp' string is unbalanced")

(defun interp--expr-end (s start)
  "Returns a cons of the position in S of the end of the
expression and the position of the end of the closing bracket.
START should be the position in S after the opening delimiter."
  (interp--with-output-buffer
    (let* ((slen (length s))
           (old-point (point-min))
           (sexps 0)
           (bstart (+ old-point start))
           ;; (oldpos bstart)
           ;; (curpos bstart)
           (expr-end bstart))
      (save-restriction
        (insert s)
        (narrow-to-region bstart (+ old-point slen))
        (goto-char (point-max))
        (condition-case err
            (progn
              (setq expr-end (scan-sexps bstart 1))
              ;; (setq curpos expr-end)
              (setq sexps 1)
              (scan-sexps expr-end 1)
              (setq sexps 2)
              (scan-sexps expr-end 1)
              ;; (while (progn
              ;;          (setq sexps (1+ sexps))
              ;;          (setq oldpos (scan-sexps curpos 1)))
              ;;   (setq curpos oldpos))
              ;; (signal 'interp-unbalanced-expression
              ;;         (list (substring s start (max 0 (1- start)))))
              (signal 'interp-parse-error (list s start)))
          (scan-error
           (if (not (eq sexps 1))
               (signal 'interp-parse-error (list s start))
             ;; (signal 'interp-not-1-form
             ;;         (list (buffer-substring bstart expr-end)))
             (-let [(_ _msg before-delim after-delim) err]
               (cond
                 ((eq (char-after before-delim) ?})
                  (cons (- expr-end old-point)
                        (- after-delim old-point)))
                 (t ;; (signal 'interp-unbalanced-expression
                    ;;         (list (substring s (max 0 (1- start)))))
                  (signal 'interp-parse-error (list s start))))))))))))

(defun interp--string-expr? (x)
  (or (stringp x)
      (and (eq (car-safe x) 'quote) (stringp (cadr x)))))

(defmacro interp--format1 (fmt s)
  (if (and (equal fmt "%s")
           (interp--string-expr? s))
      s
    `(format ,fmt ,s)))

;; `mapconcat' would be much more elegant, but this is way faster
(defun interp--output-seq-string (seq fmt seplen)
  (interp--with-output-buffer
    (mapc (lambda (y) (insert (format fmt y))) seq)
    (buffer-substring (point-min) (- (point) seplen))))

(defun interp--seq->string (seq fmt sep)
  "Return a string of the elements of SEQ, formatted with FMT, separated by SEP."
  ;; TODO test if avoiding `interp--output-seq-string' like this is faster than
  ;; just calling that regardless
  (cond
    ((or (null seq)
         ;; there is only one [] and "", so `eq' can be used
         ;; and testing `eq' a couple times is generally faster than calling `memq'
         (eq seq []) (eq seq ""))
     "")
    ((and (stringp seq) (eq sep "")
          (or (null fmt) (string= fmt "%c")))
     seq)
    (t (interp--output-seq-string seq (concat fmt sep) (length sep)))))

(defun interp--invert-case (s)
  "Invert the case of all characters in S."
  (let ((res (concat s)))
    (dotimes (i (length s))
      (let* ((ch (aref s i))
             (uc (upcase ch)))
        (aset res i (if (eq uc ch) (downcase ch) uc))))
    res))

(defun interp--unicode-char (name)
  (declare (pure t) (side-effect-free t))
  (or (gethash (upcase name) (ucs-names))
      (error "Cannot resolve the unicode character named \"%s\"" name)))

(defun interp--backslashify (s)
  (let ((start 0) strs)
    (while (string-match (rx (or string-start (not "\\")) "\\u{") s start)
      (let* ((str-end (match-beginning 0))
             (charbeg (match-end 0))
             (charend (or (string-match "}" s charbeg)
                          (signal 'interp-no-end-in-sight
                                  (list (substring s str-end)))))
             (next-str-beg (match-end 0)))
        (push (substring s start str-end) strs)
        (-> (substring s charbeg charend)
            interp--unicode-char
            string
            (push strs))
        (setq start next-str-beg)))
    (apply #'concat (nreverse (cons (substring s start) strs)))))

(defconst interp--format-regexp
  (rx "%"
      ;; flags
      (? (+? (any "+ #0-")))
      ;; width
      (? (+? digit))
      ;; precision
      (? (seq "." (+ digit)))
      ;; control character
      (any "sdoxXefgcS")))

(defconst interp--case-regexp
  (rx (or (group-n 3 "^_")
          (group-n 4 "_^")
          (and (group-n 1 "_") (or string-end (not "^")))
          (and (group-n 2 "^") (or string-end (not "_"))))))

(defconst interp--splice-regexp
  (rx "@" (? (group-n 1 (+? anything)))))

(defconst interp--quote-regexp
  (rx "'" (? (group-n 1 (+? (or "\\@" (not "@")))))))

(rx-define interp--modifiers (end)
  (1+ (or (seq "\\" end) (not end))))

(defconst interp--expr-start-regexp
  (rx (or (not "$") string-start)
      (group-n 1 "$" (? (group-n 2 (interp--modifiers "{"))) "{")))

(defun interp--divide (s)
  (save-match-data
    (let ((start 0)
          parts)
      (while (string-match interp--expr-start-regexp s start)
        (-let* ((prebeg (match-beginning 1))
                (preend (match-end 1))
                (mods (interp--match s 2 ""))
                ;; (endbeg (string-match "}" s preend))
                ;; (endend (match-end 0))
                ((expend . bracketend) (interp--expr-end s preend)))
          (push (substring s start prebeg) parts)
          (let ((ss (substring s preend expend)))
            (push (cons mods ss) parts))
          (setq start bracketend)))
      (nreverse (cons (substring s start) parts)))))

(defsubst interp--format-quote (s)
  (replace-regexp-in-string "%" "%%" s))

(defun interp--match-bounds (match &optional s regex start)
  ;; (when regex (string-match regex s start))
  (--when-let (and (or (null regex)
                       (string-match regex s start))
                   (match-beginning match))
    (cons it (match-end match))))

(defsubst interp--invalid-modifiers (modstring)
  (error "Invalid `interp' modifiers: %s" modstring))

;; TODO generalize this
(defun interp--convert (mods expression &optional start)
  (save-match-data
    (-let* ((expr (read expression))
            ((fmtstart . fmtend)
             (interp--match-bounds 0 mods interp--format-regexp start))
            ((casestart . caseend)
             (interp--match-bounds 0 mods interp--case-regexp (or fmtend start)))
            (case-fun (cond
                        ((match-beginning 1) 'downcase)
                        ((match-beginning 2) 'upcase)
                        ((match-beginning 3) 'capitalize)
                        ((match-beginning 4) 'interp--invert-case)))
            ((quotestart . quoteend)
             (interp--match-bounds 0 mods interp--quote-regexp
                                   (or caseend fmtend start)))
            (quote-infix (and quotestart (interp--match mods 1 " ")))
            (splicestart (string-match interp--splice-regexp
                                       mods (or quoteend caseend fmtend start)))
            (format-string (if fmtstart
                               (substring mods fmtstart fmtend)
                             "%s")))
      (when splicestart
        (unless (eq splicestart
                    (or quoteend caseend fmtend start 0))
          (interp--invalid-modifiers mods))
        (setq expr `(interp--seq->string
                     ,expr ,format-string
                     ,(interp--match mods 1 "")))
        (setq format-string "%s"))
      (when casestart
        (unless (eq casestart (or fmtend start 0))
          (interp--invalid-modifiers mods))
        (setq expr `(,case-fun (interp--format1 ,format-string ,expr)))
        (setq format-string "%s"))
      (when quotestart
        (unless (eq quotestart (or caseend fmtend start 0))
          (interp--invalid-modifiers mods))
        (setq format-string
              (-> (concat expression quote-infix)
                  interp--format-quote
                  (concat format-string))))
      (cons format-string expr))))

(defun interp--translate (istring &optional format-fun)
  (let ((div (interp--divide istring))
        (fmt (or format-fun 'format))
        fmtparts
        fmtargs)
    (dolist (piece div)
      (if (stringp piece)
          (push (->> (interp--backslashify piece)
                     (replace-regexp-in-string "%" "%%"))
                fmtparts)
        (-let* (((mods . expr) piece)
                ((fstr . expr) (interp--convert mods expr)))
          (push fstr fmtparts)
          (push expr fmtargs))))
    (let ((str (apply #'concat (nreverse fmtparts))))
      (if (or fmtargs format-fun)
          `(,fmt ,str ,@(nreverse fmtargs))
        str))))

;;;###autoload
(defmacro interp (istring)
  "TODO"
  (interp--translate istring))

;;;###autoload
(defmacro interp-message (istring)
  "Call `message' with the interpolated ISTRING."
  (interp--translate istring 'message))

;;;###autoload
(defmacro interp-error (istring)
  "Signal an error with the message ISTRING."
  `(signal 'error (list ,(interp--translate istring 'format-message))))

;;;###autoload
(defmacro interp-sym (istring &optional type)
  "Create a symbol named by interpolating ISTRING.
Type determines how symbol is created. It can be:

nil or omitted  - calls `make-symbol'
gensym          - calls `gensym'
soft            - calls `intern-soft'
t or intern     - calls `intern'
(soft EXPR)     - calls `intern-soft' with EXPR, which should return an obarray
EXPR            - calls `intern' with EXPR, which should return an obarray

 (--map-indexed (interp-sym \"${it}-${it-index}\") '(never eat soggy waffles))
 => (#:never-0 #:eat-1 #:soggy-2 #:waffles-3)"
  (let ((s `(interp ,istring)))
    (pcase type
      ('nil `(make-symbol ,s))
      ('gensym `(gensym ,s))
      ('soft `(intern-soft ,s))
      ((or 't 'intern) `(intern ,s))
      (`(soft ,ob) `(intern-soft ,s ,ob))
      (ob `(intern ,s ,ob)))))

(provide 'interp)
;;; interp.el ends here
