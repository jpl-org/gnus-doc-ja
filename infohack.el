;;; infohack.el --- a hack to format info file.
;; Copyright (C) 2001-2014 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: info

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(load-file (expand-file-name "ptexinfmt.el" "./"))

(defun infohack-remove-unsupported ()
  (goto-char (point-min))
  (while (re-search-forward "@\\(end \\)?ifnottex" nil t) 
    (replace-match ""))
  (goto-char (point-min))
  (while (search-forward "\n@iflatex\n" nil t)
    (delete-region (1+ (match-beginning 0))
		   (search-forward "\n@end iflatex\n"))))

(defun infohack-replace-unsupported ()
  (goto-char (point-min))
  (while (search-forward "@indicateurl{" nil t)
    (replace-match "@url{"))
  (goto-char (point-min))
  (while (search-forward "@enumerate" nil t)
    (narrow-to-region (point)
		      (or (re-search-forward "@end[\t ]+enumerate" nil t)
			  (point-max)))
    (goto-char (point-min))
    (while (re-search-forward "@item[\t ]+\\([^\t\n ]\\)" nil t)
      (replace-match "@item\n\\1"))))

(defun infohack-include-files ()
  "Insert @include files."
  (goto-char (point-min))
  (set-syntax-table texinfo-format-syntax-table)
  (let (start texinfo-command-end filename)
    (while (re-search-forward "^@include" nil t)
      (setq start (match-beginning 0)
	    texinfo-command-end (point)
	    filename (texinfo-parse-line-arg))
      (delete-region start (point-at-bol 2))
      (message "Reading included file: %s" filename)
      (save-excursion
	(save-restriction
	  (narrow-to-region
	   (point)
	   (+ (point) (car (cdr (insert-file-contents filename)))))
	  (goto-char (point-min))
	  ;; Remove `@setfilename' line from included file, if any,
	  ;; so @setfilename command not duplicated.
	  (if (re-search-forward "^@setfilename" (point-at-eol 100) t)
	      (delete-region (point-at-bol 1)
			     (point-at-bol 2))))))))

(defun infohack (file)
  (let ((dest-directory default-directory)
	(max-lisp-eval-depth (max max-lisp-eval-depth 600))
	coding-system)
    ;; Emacs 21.3 doesn't support @documentencoding
    (unless (get 'documentencoding 'texinfo-format)
      (put 'documentencoding 'texinfo-format 
	   'texinfo-discard-line-with-args))
    (find-file file)
    (setq buffer-read-only nil)
    (setq coding-system buffer-file-coding-system)
    (infohack-remove-unsupported)
    (infohack-include-files)
    (infohack-replace-unsupported)
    (texinfo-every-node-update) 
    (texinfo-format-buffer t) ;; Don't save any file.
    (setq default-directory dest-directory)
    (setq buffer-file-name 
	  (expand-file-name (file-name-nondirectory buffer-file-name)
			    default-directory))
    (setq buffer-file-coding-system coding-system)
    (if (> (buffer-size) (if (boundp 'Info-split-threshold)
			     (+ 50000 Info-split-threshold)
			   100000))
	(Info-split))
    (save-buffer)))

(eval-and-compile
  (when (string-match "windows-nt\\|os/2\\|emx\\|cygwin"
                      (symbol-name system-type))
    (defun subst-char-in-region (START END FROMCHAR TOCHAR &optional NOUNDO)
      "From START to END, replace FROMCHAR with TOCHAR each time it occurs.
If optional arg NOUNDO is non-nil, don't record this change for undo
and don't mark the buffer as really changed.
Both characters must have the same length of multi-byte form."
      (let ((original-buffer-undo-list buffer-undo-list)
            (modified (buffer-modified-p)))
        (if NOUNDO
            (setq buffer-undo-list t))
        (goto-char START)
        (let ((from (char-to-string FROMCHAR))
              (to (char-to-string TOCHAR)))
          (while (search-forward from END t)
            (replace-match to t t)))
        (if NOUNDO
            (progn (setq buffer-undo-list original-buffer-undo-list)
                   (set-buffer-modidifed-p modified)))))))

(defun batch-makeinfo ()
  "Emacs makeinfo in batch mode."
  (infohack-texi-format (car command-line-args-left)
			(car (cdr command-line-args-left)))
  (setq command-line-args-left nil))


(require 'bytecomp)

;; Reduce the number of split Info files.
(unless (boundp 'Info-split-threshold)
  (if (featurep 'xemacs)
      (if (load "informat.el" t t)
	  (let* ((fn (symbol-function 'Info-split))
		 (fns (prin1-to-string fn)))
	    (load "informat.elc" t t)
	    (when (and (string-match "\\([\t\n ]+\\)50000\\([\t\n )]+\\)" fns)
		       (condition-case nil
			   (setq fn (byte-compile
				     (read (replace-match "\\1262144\\2"
							  nil nil fns))))
			 (error nil))
		       (fset 'Info-split fn)))))
    (require 'informat)
    (let* ((fn (symbol-function 'Info-split))
	   (fns (prin1-to-string fn)))
      (when (string-match "\\([\t\n ]+\\)50000\\([\t\n ]+\\)" fns)
	(condition-case nil
	    (fset 'Info-split (read (replace-match "\\1262144\\2"
						   nil nil fns)))
	  (error
	   (fset 'Info-split fn)))))))

(defun infohack-texi-format (file &optional addsuffix)
  (let ((auto-save-default nil)
	(find-file-run-dired nil)
	(coding-system-for-read (if (featurep 'xemacs)
				    'iso-2022-7bit
				  coding-system-for-read))
	coding-system-for-write
	(error 0))
    (condition-case err
	(progn
	  (find-file file)
	  (setq buffer-read-only nil)
	  (buffer-disable-undo (current-buffer))
	  (setq coding-system-for-write buffer-file-coding-system)
	  ;; Remove ignored areas.
	  (goto-char (point-min))
	  (while (re-search-forward "^@ignore[\t\r ]*$" nil t)
	    (delete-region (match-beginning 0)
			   (if (re-search-forward
				"^@end[\t ]+ignore[\t\r ]*$" nil t)
			       (1+ (match-end 0))
			     (point-max))))
	  ;; Remove unsupported commands.
	  (infohack-remove-unsupported)
	  ;; Add suffix if it is needed.
	  (goto-char (point-min))
	  (when (and addsuffix
		     (re-search-forward "^@setfilename[\t ]+\\([^\t\n ]+\\)"
					nil t)
		     (not (string-match "\\.info$" (match-string 1))))
	    (insert ".info"))
	  (infohack-include-files)
	  (texinfo-every-node-update)
	  (set-buffer-modified-p nil)
	  (message "texinfo formatting %s..." file)
	  (let ((si:message (symbol-function 'message))
		(coding (or (and (string-match "\\`Japanese"
					       current-language-environment)
				 (boundp 'locale-coding-system)
				 locale-coding-system)
			    'iso-2022-7bit)))
	    ;; Encode messages to terminal.
	    (fset
	     'message
	     (byte-compile
	      (if (featurep 'xemacs)
		  `(lambda (fmt &rest args)
		     (unless (and (string-equal fmt "%s clean")
				  (equal (car args) buffer-file-name))
		       (funcall ,si:message "%s"
				(encode-coding-string (apply 'format fmt args)
						      ',coding))))
		`(lambda (fmt &rest args)
		   (funcall ,si:message "%s"
			    (encode-coding-string (apply 'format fmt args)
						  ',coding))))))
	    (unwind-protect
		(texinfo-format-buffer nil)
	      (fset 'message si:message)))
	  (if (buffer-modified-p)
	      (progn (message "Saving modified %s" (buffer-file-name))
		     (save-buffer))))
      (error
       (message ">> Error: %s" (prin1-to-string err))
       (message ">>  point at")
       (let ((s (buffer-substring (point) (min (+ (point) 100) (point-max))))
	     (tem 0))
	 (while (setq tem (string-match "\n+" s tem))
	   (setq s (concat (substring s 0 (match-beginning 0))
			   "\n>>  "
			   (substring s (match-end 0)))
		 tem (1+ tem)))
	 (message ">>  %s" s))
       (setq error 1)))
    (kill-emacs error)))

;;; infohack.el ends here
