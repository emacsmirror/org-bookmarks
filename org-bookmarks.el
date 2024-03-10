;;; org-bookmarks.el --- Manage bookmarks in Org mode -*- lexical-binding: t; -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

;; Authors: stardiviner <numbchild@gmail.com>, Nicholas Vollmer <progfolio@protonmail.com>
;; Package-Requires: ((emacs "26.1") (ivy "0.14.2") (nerd-icons "0.1.0"))
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
(require 'org-capture)
(require 'nerd-icons)

(defgroup org-bookmarks nil
  "The defcustom group of `org-bookmarks'."
  :prefix "org-boomarks-"
  :group 'org)

;; The :group keyword is not necessary if a group has been defined in the same file.
;; It will default to the last declared.

;; It would be better to default to the demo file in the repository.
;; That avoids creating a directory on the user's system that may not exist yet.
;; It also allows the user to try the command out without customizing anything first.
(defcustom org-bookmarks-file
  (expand-file-name "bookmarks.org" (file-name-directory (or load-file-name (buffer-file-name))))
  "The Org bookmarks filename."
  :type 'string
  :safe #'stringp
  :group 'org-bookmarks)

(defcustom org-bookmarks-tag "bookmark"
  "The tag to mark Org headline as bookmark entry."
  :type 'string
  :safe #'stringp
  :group 'org-bookmarks)

;; It's possible the user may want to change which function is used to browse the URL.
(defcustom org-bookmarks-browse-function #'browse-url
  "Function called by `org-bookmarks' with selected URL as its sole argument."
  :type 'function)

;; Decomposing the logic of the main funciton into smaller functions makes the program
;; easier to reason about and more flexible.

;; We're mapping over headlines via the org-element API, so we can assume that's
;; what type of element we'll be operating on here.
(defun org-bookmark--candidate (headline)
  "Return candidate string from Org HEADLINE."
  ;; We can use when-let to succinctly assign variables and return early for
  ;; non-matching headlines
  (when-let ((tags (org-element-property :tags headline))
             ((member org-bookmarks-tag tags))
             (url (alist-get "URL" (org-entry-properties headline 'standard) nil nil #'equal))
             (info (concat (unless (= (length tags) 1)
                             (format "[%s]" (string-join (delete org-bookmarks-tag tags) ":")))
                           "\n" (propertize url 'face 'link) "\n")))
    ;; The URL and ANNOTATION properties will be used for candidate display and browsing.
    (propertize (org-element-property :raw-value headline) 'url url 'annotation info)))

(defun org-bookmark--candidates (file)
  "Return a list of candidates from FILE."
  ;; It's better to use a temp buffer than touch the user's buffer.
  ;; It also cleans up after itself.
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks ;; This will prevent user hooks from running during parsing.
      (org-mode)
      (goto-char (point-min))
      (let ((candidates nil))
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (headline)
            (when-let ((candidate (org-bookmark--candidate headline)))
              (push candidate candidates))))
        (nreverse candidates)))))

;; The annotation function can look up the properties on each candidate.
(defun org-bookmarks--annotator (candidate)
  "Annotate bookmark completion CANDIDATE."
  (concat (propertize " " 'display '(space :align-to center))
          (get-text-property 0 'annotation candidate)))

(defun org-bookmarks (&optional file)
  "Open bookmark read from FILE or `org-bookmarks-file'."
  (interactive)
  (if-let ((file (or file org-bookmarks-file))
           ;; Ensure file exists first.
           ((file-exists-p file)))
      (if-let ((candidates (org-bookmark--candidates file))
               ;; Necessary for propertized text in minibuffer.
               (minibuffer-allow-text-properties t)
               (completion-extra-properties
                ;; Using the "bookmark" category caused the annotations to not show.
                ;; I think that may have be do to vertico-mode, but
                ;; it's probably worth using a unique category so users can exercise finer-grained  customization.
                (list :category 'org-bookmark
                      :annotation-function #'org-bookmarks--annotator))
               (choice (completing-read "org-bookmarks: " candidates nil 'require-match))
               (url (get-text-property 0 'url choice)))
          (funcall org-bookmarks-browse-function url)
        (user-error "No bookmarks found in %S" file))
    (user-error "File does not exist: %S" file)))

;;; TEST:
;; (org-bookmarks "bookmarks.org")
;; (org-bookmarks (expand-file-name org-bookmarks-file))

;;; Add `org-capture' template for adding new bookmark to `org-bookmarks-file'
(unless (assoc "b" org-capture-templates)
  (add-to-list
   'org-capture-templates
   `("b" ,(format "%s\tAdd a new bookmark to %s"
                  (when (featurep 'nerd-icons)
                    (nerd-icons-mdicon "nf-md-bookmark_plus_outline" :face 'nerd-icons-blue))
                  org-bookmarks-file)
     entry (file ,(expand-file-name org-bookmarks-file))
     "* %^{bookmark title}
:PROPERTIES:
:URL:  %^C
:DATE: %t
:END:"
     :empty-lines 1
     :jump-to-captured t)
   :append))



(provide 'org-bookmarks)

;;; org-bookmarks.el ends here
