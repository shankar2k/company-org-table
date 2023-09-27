;;; company-org-table.el --- Autocomplete Org table cells using company  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Shankar Rao

;; Author: Shankar Rao <shankar.rao@gmail.com>
;; URL: http://example.com/company-org-table.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: org

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package contains my various custom functions for org-mode.

;;;; Installation

;;;;; MELPA

;; I would be surprised if this somehow found its way into MELPA.

;;;;; Manual

;; First install company, then put this file in your load-path, and put this
;; in your init file:

;; (require 'company-org-table)
;; (add-to-list 'company-backends 'company-org-table)

;;;; Usage

;; This package defines the following commands:
;;
;; `company-org-table': `company-mode' backend for Org tables that mimics the
;; auto-complete behavior of spreadsheet programs like Excel, LibreOffice, and
;; Google Sheets.

;;;; Credits

;; This package would not have been possible without the following guides for
;; writing company backends: Company Github repository [1] and the Sixty North
;; blog [2].
;;
;;  [1] https://github.com/company-mode/company-mode/wiki/Writing-backends
;;  [2] http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'org)
(require 'company)

;;;; Customization

(defcustom company-org-table-section 'exclude
  "Section of Org table column at point to use for completion candidates.

The value `exclude' uses all cells except the cell at
point. The value `above' uses column cells above point,
and the value `below' uses column cells below point."
  :type '(choice
          (const :tag "Use all except cell at point for completion" 'exclude)
          (const :tag "Use cells above point for completion" 'above)
          (const :tag "Use cells below point for completion" 'below)))

;;;; Constants / Variables

(defvar company-org-table-prefix-length 0
  "Length of prefix string to be completed.")

(defvar company-org-table-right-distance 0
  "Distance between point and right bar of table cell where completion is occurring.")

(defvar company-org-table-alist nil
  "Alist mapping table header information to candidate list generators.

Each key is a two-element list where the first element is a
regexp matching an Org table name, and the second element is a
regexp matching a column header.

Each value is a function with no arguments that returns a list of
completion candidates.")

;;;;; Keymaps

;;;; Functions

(defun company-org-table (command &optional arg &rest ignored)
  "`company-mode' backend that completes an Org table cell."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-table))
    (prefix (and (org-at-table-p) (company-org-table-prefix)))
    (candidates (company-org-table-candidates arg))
    (post-completion (company-org-table-post-completion arg))))

;;;;; Support

;;;;;; Backend Helper Functions

(defun company-org-table-prefix ()
  "Prefix command used by `company-org-table'.

This function assumes the point in an Org table and returns the
text before the point in the current table cell."
  (unless (or (= (following-char) ?|) (looking-back "|[ \t]*")))
    (let ((pt (point)))
      (save-excursion
        (re-search-backward "| ?" nil t)
        (goto-char (match-end 0))
        (skip-chars-forward " \t")
        (buffer-substring (point) pt))))


(defun company-org-table-candidates (prefix)
  "Generate a list of completion candidates that start with PREFIX.

This records the length of prefix in
`company-org-table-prefix-length' and distance to the right end
of table cell in `company-org-table-right-distance' so that they
can be accessed during post-completion."
  (setq company-org-table-prefix-length (length prefix)
        company-org-table-right-distance
        (- (save-excursion (search-forward "|" nil t)) (point) 2))
  (let ((prefix-re (concat (rx bos) (char-fold-to-regexp prefix)))
        (cand-list (funcall (alist-get (company-org-table-name-header)
                                       company-org-table-alist
                                       #'company-org-table-candidates-column
                                       nil
                                       #'company-org-table-match))))
    (cl-remove-if-not (lambda (cand) (string-match prefix-re cand)) cand-list)))

(defun company-org-table-post-completion (cand)
  "Post-completion command for `company-org-table' backend.

This deletes extra spaces caused by insertion of the candidate into the table."
  (delete-char (min (- (length cand) company-org-table-prefix-length)
                    company-org-table-right-distance)))

(defun company-org-table-name-header ()
  "Get the name and column header of Org table at point as a list."
  (let ((curr-table (org-element-lineage (org-element-at-point)
                                         '(table) t)))
    (list (or (org-element-property :name curr-table) "")
          (save-excursion
            (goto-char (+ (org-element-property :contents-begin curr-table)
                          (current-column)))
            (org-trim (substring-no-properties (org-table-get-field)))))))


(defun company-org-table-get-column (&optional section)
  "Get contents of SECTION of Org table column at point as a list.

If SECTION is nil or the symbol `all', get all column cells. If
SECTION is the symbol `above', get all columns cells above the
point. If SECTION is the symbol `below', get all column cells
below the point. If SECTION is any other symbol (e.g.,
`exclude'), get all column cells except for the cell at point."
  (when (org-at-table-p)
    (save-excursion
      (let* ((section (or section 'all))
             (goal-column (progn
                            (re-search-backward "| ?" nil t)
                            (goto-char (match-end 0))
                            (current-column)))
             (pt (point))
             (current (when (and (eq section 'all)
                                 (not (org-at-table-hline-p)))
                        (list (company-org-table--get-field))))
             (above (unless (eq section 'below)
                      (company-org-table--get-part -1)))
             (below (unless (eq section 'above)
                      (goto-char pt)
                      (nreverse (company-org-table--get-part +1)))))
        (nconc above current below)))))

;;;;;; Argument Functions

(defun company-org-table-match (re key)
  "Non-nil if each key in KEY matches each corresponding regexp in RE.

This is used as a test function to search `company-org-table-alist'."
  (seq-every-p #'identity (seq-mapn #'string-match re key)))


(defun company-org-table-candidates-column ()
  "Get list of candidates from a section of Org table column at point.

The section is specified by `company-org-table-section'. The
candidates are filtered to remove redundant elements and the
column header is ignored. This function is used to obtain a
default set of candidates if searching `company-org-table-alist'
return nil."
  (seq-uniq (cdr (company-org-table-get-column company-org-table-section))))

;;;;;; Private Helper Functions

(defun company-org-table--get-field ()
  "Get text in Org table cell at point.

This is a helper function used by
`company-org-table--get-column'. It assumes that the point is on
the first character of cell text."
  (skip-chars-forward " \t")
  (buffer-substring
   (point)
   (progn
     (re-search-forward "[ \t]*\\(|\\|$\\)")
	 (match-beginning 0))))


(defun company-org-table--get-part (arg)
  "Get part of Org table column at point as a list.

This is a helper function used by
`company-org-table--get-column'. When ARG is -1, return the
column fields above the point, and when ARG is +1, return the
column fields below the point. This function assumes that
`goal-column' has been set to the current Org table column."
  (let ((sdata nil))
    (while (and (line-move arg t) (org-at-table-p))
      (unless (org-at-table-hline-p)
        (push (company-org-table--get-field) sdata)))
    sdata))

;;;; Footer

(provide 'company-org-table)

;;; company-org-table.el ends here
