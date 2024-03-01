;;; org-bookmarks.el --- Manage bookmarks in Org mode -*- lexical-binding: t; -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "26.1") (ivy "0.14.2"))
;; Version: 0.1
;; Keywords: outline matching hypermedia org
;; URL: https://repo.or.cz/org-bookmarks.git

;; org-bookmarks is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; org-bookmarks is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage

;;; Code:

(require 'org-element)
(require 'seq)
(require 'ivy)

(defgroup org-bookmarks nil
  "The defcustom group of `org-bookmarks'."
  :prefix "org-boomarks-"
  :group 'org)

(defcustom org-bookmarks-file "~/Org/Bookmarks/Bookmarks.org"
  "The Org bookmarks file."
  :type 'string
  :safe #'stringp
  :group 'org-bookmarks)

(defcustom org-bookmarks-tag "bookmark"
  "The tag to mark Org headline as bookmark entry."
  :type 'string
  :safe #'stringp)

(defun org-bookmarks (&optional org-file)
  "Search bookmarks in Org mode bookmarks file ORG-FILE.

This command is entry of package \"org-bookmarks.el\".
You can set ORG-FILE with option `org-bookmarks-file'."
  (interactive)
  (let ((bookmarks-file (or org-file org-bookmarks-file)))
    (with-current-buffer (or (get-buffer (file-name-nondirectory bookmarks-file))
                             (find-file-noselect bookmarks-file))
      (let ((bookmark-headlines))
        (org-element-map (org-element-parse-buffer) 'headline ; FIXME: parsed headlines not correct.
          (lambda (node)
            (let ((headline node))
              (when (member org-bookmarks-tag (org-element-property :tags headline))
                (let* ((element (or headline (org-element-context)))
                       (headline-text (org-element-property :raw-value element))
                       (tags (delete "bookmark" (org-element-property :tags element)))
                       (properties (org-entry-properties element 'standard))
                       (property-url (cdr (assoc "URL" properties))))
                  (push (when (and headline-text property-url) ; make sure those variable not nil.
                          (concat headline-text (when tags (concat "   [" (string-join tags ":") "] ")) "\n"
                                  (propertize property-url 'face 'link) "\n"))
                        bookmark-headlines))))))
        (if bookmark-headlines
            (browse-url
             (substring-no-properties
              (if (and (featurep 'ivy) ivy-mode)
                  (seq-elt (split-string (ivy-read "org-bookmarks: " bookmark-headlines) "\n") 1)
                (let ((completion-extra-properties
                       (list :category 'bookmark
                             :annotation-function (lambda (bookmark-headline)
                                                    ;; FIXME: how to display headline & url as two-line candidate format.
                                                    (let ((url (cdr (split-string bookmark-headline "\n"))))
                                                      (concat (propertize " " 'display '(space :align-to center))
		                                                      (get-text-property 0 :annotation candidate)
                                                              (format " (%s)" url)))))))
                  (seq-elt (split-string (completing-read "org-bookmarks: " bookmark-headlines) "\n") 1)))))
          (message "[org-bookmarks] WARNING: Have not found any bookmarks!"))))))

;;; TEST:
;; (org-bookmarks "bookmarks.org")
;; (org-bookmarks (expand-file-name org-bookmarks-file))



(provide 'org-bookmarks)

;;; org-bookmarks.el ends here
