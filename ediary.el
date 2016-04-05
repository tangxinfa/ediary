;;; ediary.el --- a diary publish system.

;; Copyright (C) 2015  tangxinfa

;; Author: tangxinfa <tangxinfa@gmail.com>
;; Maintainer: tangxinfa <tangxinfa@gmail.com>
;; Version: 0.0.1
;; Keywords: diary, blog, hypermedia
;; URL: https://github.com/tangxinfa/ediary.git

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; `ediary-export' Export to ediary file.
;; `ediary-build' Build web pages from ediary file.
;; `ediary-publish' Export and build web pages from ediary file.

;;; Code:

(require 'cl)
(require 'org)

;;;; Customization variables

(defcustom ediary-export-options "H:7 num:nil  toc:nil d:nil todo:nil <:nil pri:nil tags:nil"
  "org-mode export options."
  :group 'ediary)

(defcustom ediary-export-file (concat (el-get-package-directory 'ediary) "source/ediary.json")
  "ediary file exported."
  :group 'ediary)

(defcustom ediary-source-file (concat (el-get-package-directory 'ediary) "source/ediary.org")
  "ediary file composed."
  :group 'ediary)

;;;;;
;;;;; EDIARY ENTRY STRUCTURE:
;;;;;    - title string.
;;;;;    - timestamp string. format: YYYY-MM-DD HH:mm:ss
;;;;;    - tags list. (tag ...)
;;;;;    - body string. format: html
;;;;;

(defun ediary-entry-at (marker)
  "Parse ediary entry MARKER points to."
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker))
    (when (search-forward-regexp org-complex-heading-regexp (point-at-eol) t)
      (let ((title (match-string-no-properties 4))
            (timestamp (org-parse-time-string (org-entry-get (point) "CLOSED")))
            (tags (org-get-local-tags))
            (body nil)
            (entry nil)
            (body-begin nil))
        (setq timestamp (format "%04d-%02d-%02d %02d:%02d:%02d" (nth 5 timestamp) (nth 4 timestamp) (nth 3 timestamp) (nth 2 timestamp) (nth 1 timestamp) (nth 0 timestamp)))
        (search-forward-regexp "\\(^\\s-*$\\)\n") ;; after the empty line is body
        (setq body-begin (point))
        (goto-char (marker-position marker))
        (org-end-of-subtree)
        (setq body (concat "#+OPTIONS: " ediary-export-options "\n\n" (buffer-substring-no-properties body-begin (point))))
        (setq entry (plist-put entry 'title title))
        (setq entry (plist-put entry 'timestamp timestamp))
        (setq entry (plist-put entry 'tags tags))
        (setq entry (plist-put entry 'body (org-export-string-as body 'html t)))
        ))))

(defun ediary-entries ()
  "Parse ediary entries of current buffer."
  (let* ((markers (org-map-entries 'point-marker "+TODO=\"DONE\"")))
    (loop for marker in markers
          collect (ediary-entry-at marker) into entries
          finally return entries)))

(defun ediary-export ()
  "Export ediary-source-file into file: ediary-export-file"
  (interactive)
  (let ((entries (ediary-entries))
        (json-encoding-pretty-print t)
        (next-entry nil))
      (with-temp-file ediary-export-file
        (insert "[")
        (dolist (entry entries)
          (if next-entry
              (progn
                (insert ",\n")))
          (setq next-entry t)
          (insert (json-encode-plist entry)))
        (insert "]"))))

(defun ediary-build ()
  "Build web pages from ediary file: ediary-export-file"
  (interactive)
  (async-shell-command
   (format "%s -f %s" ediary-build-tool (shell-quote-argument (expand-file-name ediary-export-file)))
   "*ediary*"
   "*ediary*"))

(defun ediary-publish ()
  "Export and build web pages from ediary source file: ediary-source-file"
  (interactive)
  (find-file ediary-source-file)
  (let ((default-directory (expand-file-name (concat (file-name-directory ediary-source-file) "../")))
        (org-export-with-sub-superscripts nil))
    (ediary-export)
    (ediary-build)))

(defun ediary-source (&optional local)
  "Open composed ediary source file: ediary-source-file"
  (interactive "P")
  (when local
    (message "update ediary source ...")
    (let ((default-directory "~"))
      (let* ((command-output (shell-command-to-string (format "cd %s; git pull" (file-name-directory ediary-source-file)))))
        (if (string-match "error:" command-output)
            (error "%s" command-output)
          (message "%s" command-output)))))
  (find-file-existing ediary-source-file)
  (goto-char (point-max)))

(provide 'ediary)
;;; ediary.el ends here.
