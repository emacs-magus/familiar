;;; familiar.el --- A more convenient key definer -*- lexical-binding: t; -*-

;; Author: Fox Kiester <https://github.com/noctuid>
;; URL: https://github.com/emacs-magus/familiar
;; Created: May 02, 2020
;; Keywords: convenience keybindings keys config startup dotemacs
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A more convenient key definer.
;;

;; For more information see the README in the online repository.

;;; Code:
(require 'familiar-usul)

(defcustom familiar-dwim nil
  "Whether `familiar' will allow guessing which arguments are positional.
If nil, : or a keyword is required to separate any positional arguments from
keybindings.  If non-nil, symbols and lists (quoted or unquoted) will be
considered positional arguments, and the start of the keybindings should be
marked by a string/vector key."
  :type 'boolean)

(defun familiar--positional-arg-p (arg)
  "Return whether ARG is a positional argument for a key definer.
Symbols and lists (quoted or unquoted) are considered positional arguments.
Keyword arguments and strings/vectors are not considered positional arguments."
  (and arg
       (not (keywordp arg))
       (or (not familiar-dwim)
           (or (symbolp arg) (listp arg)))))

(defun familiar--normalize-positional-arg (arg)
  "Return ARG but quoted if it is not quoted."
  (if (eq (car-safe arg) 'quote)
      arg
    `',arg))

(defun familiar--default-parse-positional-argument (positional-args)
  "If POSITIONAL-ARGS convert it to a plist that sets :keymaps to the value."
  (when (= (length positional-args) 1)
    (cons :keymaps positional-args)))

(defvar familiar--positional-argument-parsers
  (list #'familiar--default-parse-positional-argument)
  "List of positional argument parsers for `familiar'.
Each one will be called with the list of positional arguments as a single
argument until one returns non-nil.  The return value should be a list of
keyword arguments.")

(defun familiar--convert-positional-args-to-kargs (positional-args)
  "Convert POSITIONAL-ARGS to a list of keyword arguments.
Try each parser in `familiar--positional-argument-parsers' until one returns
non-nil."
  (cl-dolist (parser familiar--positional-argument-parsers)
    (when-let ((kargs (funcall parser positional-args)))
      (cl-return kargs))))

(defun familiar--parse-arglist (args)
  "Parse ARGS to form a single `familiar--define-key' arglist.
Return (list <kargs> <keybinds> <remaining-args>).  The remaining args will be a
list of unprocessed arguments that correspond to 1+ separate
`familiar--define-key' arglists."
  (let (arg
        positional-args
        kargs
        keybinds
        all-extended)
    (while (familiar--positional-arg-p (car args))
      (push (familiar--normalize-positional-arg (pop args)) positional-args))

    (when positional-args
      (setq kargs
            (nreverse
             (familiar--convert-positional-args-to-kargs positional-args))))

    (when (eq (car args) :)
      (pop args))
    (while (and (keywordp (car args))
                (not (eq (car args) :)))
      (push (pop args) kargs)
      (push (pop args) kargs))

    (while (and (setq arg (car args))
                (or (not (keywordp arg))
                    (eq arg :ext)))
      (let ((key (pop args))
            (val (if all-extended
                     (car args)
                   (pop args))))
        (cond ((and (eq key :ext)
                    (memq val '(t nil)))
               (setq all-extended val))
              ((eq key :ext)
               (push (cons 'list val) keybinds))
              (t
               (if all-extended
                   (push (cons 'list key) keybinds)
                 (push (list 'list key val) keybinds))))))

    (list (nreverse kargs) (nreverse keybinds) args)))

(defun familiar--merge-plists (a b)
  "Return the result of merging the plists A and B.
When the same property exists in both A and B, prefer A's value."
  (let ((res (cl-copy-list a)))
    (cl-loop for (prop val) on b by #'cddr
             unless (plist-get res prop)
             do (setq res (plist-put res prop val)))
    res))

(defun familiar--parse-combined-arglists (arglist)
  "Parse the unevaluated ARGLIST into multiple `familiar--define-key' arglists."
  (let (parsed-arglists
        accumulated-kargs)
    (while arglist
      (let ((next-parsed-arglist (familiar--parse-arglist arglist)))
        (setq accumulated-kargs
              (familiar--merge-plists (car next-parsed-arglist)
                                      accumulated-kargs))
        ;; can specify default keywords and no keybinds, ignore if no keybinds
        (when-let ((keybinds (cadr next-parsed-arglist)))
          (push (cons (cons 'list accumulated-kargs)
                      keybinds)
                parsed-arglists))
        (setq arglist (cl-caddr next-parsed-arglist))
        (cl-case (car arglist)
          (: (pop arglist))
          (::
           (pop arglist)
           (setq accumulated-kargs nil)))))
    (nreverse parsed-arglists)))

;;;###autoload
(defmacro familiar (&rest args)
  (declare (indent defun))
  (let ((parsed-arglists (familiar--parse-combined-arglists args)))
    `(progn
       ,@(cl-loop for arglist in parsed-arglists
                  collect `(familiar--define-key ,@arglist)))))

;;;###autoload
(defmacro familiar-create-definer (name &rest defaults)
  "Create a key definer over `familiar' called NAME.
DEFAULTS should be default keyword arguments for the definer (e.g. a default
keymap or prefix)."
  (declare (indent defun))
  `(defmacro ,name (&rest args)
     (declare (indent defun))
     ,(let ((print-quoted t))
        (format
         "A wrapper to run `familiar' with ARGS.

It has the following defaults:
%s"
         defaults))
     `(familiar : ,@',defaults : ,@args)))

(provide 'familiar)
;;; familiar.el ends here
