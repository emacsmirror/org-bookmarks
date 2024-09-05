;;; org-bookmarks.el --- Manage bookmarks in Org mode -*- lexical-binding: t; -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2024-2025 stardiviner <numbchild@gmail.com>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "29.1") (nerd-icons "0.1.0"))
;; Version: 1.2
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
;; License for more details. https://www.gnu.org/licenses/gpl-3.0.txt
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage

;; 0. config example:
;;
;; (use-package org-bookmarks
;;   :ensure t
;;   :custom ((org-bookmarks-file "~/Org/Bookmarks/Bookmarks.org")
;;            (org-bookmarks-add-org-capture-template t))
;;   :commands (org-bookmarks)
;;   :init (org-bookmarks-add-org-capture-template))

;; 1. Record bookmark information into Org mode file.
;;
;; 2. bookmark entry is recorded with format like bellowing:
;;
;;    #+begin_src org
;;    * bookmark title                                         :bookmark:tag1:tag2:
;;    :PROPERTIES:
;;    :URL:      https://www.example.com
;;    :DESCRIPTION: example url
;;    :END:
;;    
;;    #+end_src
;;
;; 3. execute command `org-bookmarks' to search and select bookmark to open in web browser.

;;; Code:

(require 'org) ; for `org-tags-column'
(require 'org-element)
(require 'nerd-icons)

(eval-when-compile (require 'org-capture))
(eval-when-compile (require 'nerd-icons nil t))
(declare-function 'nerd-icons-mdicon "nerd-icons" (icon-name &rest args))

(defgroup org-bookmarks nil
  "The defcustom group of `org-bookmarks'."
  :prefix "org-boomarks-"
  :group 'org)

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

(defcustom org-bookmarks-tag-exclude-list `(,org-archive-tag "deprecated")
  "The list of Org tags to exclude in searching bookmarks list."
  :type 'list
  :safe #'listp
  :group 'org-bookmarks)

(defcustom org-bookmarks-browse-function #'browse-url
  "Function called by `org-bookmarks' with selected URL as its sole argument."
  :type 'function
  :group 'org-bookmarks)

(defcustom org-bookmarks-add-org-capture-template nil
  "Add `org-capture' template for `org-bookmarks'.
WARNING: If you have `org-capture' template bind to key \"b\" already,
when this option is t, it will override your org-capture template.
Or you can add org-capture template by yourself."
  :type 'boolean
  :safe #'booleanp
  :group 'org-bookmarks)


(defun org-bookmarks--entry-screenshot (headline)
  "Return the bookmark HEADLINE object's webpage screenshot inline image."
  ;; limit in current headline entry.
  (let ((entry-begin (org-element-begin headline))
        (entry-end (org-element-end headline)))
    ;; skip over to property end
    (goto-char entry-begin)
    (search-forward-regexp org-property-end-re nil t)
    (when (re-search-forward org-link-any-re entry-end t)
      ;; back to link beginning
      (goto-char (match-beginning 0))
      (let* ((link-element (org-element-link-parser))
             (link-type (org-element-property :type link-element))
             (link-path (org-element-property :path link-element)))
        (when (buffer-narrowed-p) (widen))
        (when (and link-path
                   (string-equal link-type "file")
                   (member (intern (file-name-extension link-path)) image-types))
          (propertize " "
                      'display (create-image link-path nil nil :max-height (* (default-font-height) 20))))))))

;;; TEST: (org-bookmarks--entry-screenshot (org-element-context))

(defun org-bookmarks--candidate (headline)
  "Return candidate string from Org HEADLINE."
  (when-let* ((tags (org-element-property :tags headline))
              ( (and (member org-bookmarks-tag tags)
                     (not (seq-intersection tags org-bookmarks-tag-exclude-list))))
              (url (alist-get "URL" (org-entry-properties headline 'standard) nil nil #'equal))
              (description (string-fill
                            (or (alist-get "DESCRIPTION" (org-entry-properties headline 'standard) nil nil #'equal) "")
                            fill-column))
              ;; bookmark extra info as bookmark completion candidate annotation.
              (info (concat "\n" ; candidate display extra info in multi-line with "\n"
                            "   " (propertize url 'face 'link) "\n" ; property :URL:
                            "   " (propertize description 'face 'font-lock-comment-face) "\n" ; property :DESCRIPTION:
                            ;; The screenshot inline image in bookmark entry body.
                            (org-bookmarks--entry-screenshot headline) "\n"))
              (headline-title (org-element-property :raw-value headline)))
    ;; The URL and ANNOTATION properties will be used for candidate display and browsing.
    (let* ((tags-searchable (delete org-bookmarks-tag tags))
           ;; TODO: The length counting method not correct on Chinese.
           (middle-line-length (when-let* ((length (- (- org-tags-column)
                                                      (length (string-join tags-searchable ":"))
                                                      (length headline-title) 2))
                                           ((wholenump length)))
                                 length))
           (middle-line (make-string (or middle-line-length 0) ?―))
           (icon (nerd-icons-icon-for-url url))
           (tags-displaying (if (= (length tags-searchable) 1)
                                (car tags-searchable)
                              (string-join tags-searchable ":"))))
      (propertize (format " %s %s %s [%s]" icon headline-title middle-line tags-displaying)
                  'title headline-title 'url url 'annotation info))))

(defun org-bookmarks--candidates (file)
  "Return a list of candidates from FILE."
  ;; It's better to use a temp buffer than touch the user's buffer.
  ;; It also cleans up after itself.
  (with-temp-buffer
    (insert-file-contents file) ; don't need to actually open file.
    (delay-mode-hooks ; This will prevent user hooks from running during parsing.
      (org-mode)
      (goto-char (point-min))
      (let ((candidates nil))
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (headline)
            (when-let ((candidate (org-bookmarks--candidate headline)))
              (push candidate candidates))))
        (nreverse candidates)))))

(defun org-bookmarks--completion-annotator (candidate)
  "Annotate bookmark completion CANDIDATE."
  (concat (propertize " " 'display '(space :align-to center))
          (get-text-property 0 'annotation candidate)))

(defun org-bookmarks--completion-action (candidate status)
  "The action function to be executed on selected completion CANDIDATE in STATUS."
  (message "[org-bookmarks] completion `%s' is selected with status %s" candidate status))

(defun org-bookmarks--return-candidates (&optional file)
  "Return `org-bookmarks' candidates which parsed from FILE."
  (if-let ((file (or file org-bookmarks-file)))
      (org-bookmarks--candidates file)
    (user-error "File does not exist: %S" file)))

(defvar org-bookmarks--candidates-cache nil
  "A cache variable of `org-bookmarks--candidates'.")

(defun org-bookmarks-db-update-cache ()
  "Update the `org-bookmarks' database cache."
  (interactive)
  (setq org-bookmarks--candidates-cache (org-bookmarks--return-candidates)))

;;; Auto update org-bookmarks database cache in Emacs idle timer.
(defcustom org-bookmarks-db-update-idle-interval (* 60 10)
  "The idle interval seconds for update `org-bookmarks' database cache."
  :type 'number
  :safe #'numberp
  :group 'org-bookmarks)

(run-with-idle-timer org-bookmarks-db-update-idle-interval t 'org-bookmarks-db-update-cache)

;;;###autoload
(defun org-bookmarks (&optional file)
  "Open bookmark read from FILE or `org-bookmarks-file'."
  (interactive)
  (unless org-bookmarks--candidates-cache
    (org-bookmarks-db-update-cache))
  (if-let* ((file (or file org-bookmarks-file))
            ( (file-exists-p file)))
      (if-let* ((candidates org-bookmarks--candidates-cache)
                (minibuffer-allow-text-properties t)
                (completion-extra-properties
                 ;; Using the "bookmark" category caused the annotations to not show.
                 ;; I think that may have be do to vertico-mode, but it's
                 ;; probably worth using a unique category so users can exercise
                 ;; finer-grained customization.
                 (list :category 'org-bookmark
                       :annotation-function #'org-bookmarks--completion-annotator
                       :exit-function #'org-bookmarks--completion-action))
                (choice (completing-read "org-bookmarks: " candidates nil 'require-match))
                (url (get-text-property 0 'url choice)))
          (funcall org-bookmarks-browse-function url)
        (user-error "No bookmarks found in %S" file))
    (user-error "File does not exist: %S" file)))

;;; TEST:
;; (org-bookmarks "bookmarks.org")
;; (org-bookmarks (expand-file-name org-bookmarks-file))

;;; Add link type `org-bookmark:'
(defun org-bookmarks-link-open (bookmark-title _)
  "Open the \"org-bookmark:\" link type with BOOKMARK-TITLE."
  (if-let* ((file org-bookmarks-file)
            (buffer (or (get-buffer (file-name-nondirectory file))
                        (find-file-noselect file))))
      (with-current-buffer buffer
        (let ((marker (org-find-exact-headline-in-buffer bookmark-title buffer)))
          (if (fboundp 'org-goto-marker-or-bmk)
              (org-goto-marker-or-bmk marker)
            (goto-char (marker-position marker))))
        (display-buffer buffer '(display-buffer-below-selected))
        (when (or (org-invisible-p) (org-invisible-p2))
	      (org-fold-show-context)))))

(defun org-bookmarks-link-store (&optional _interactive?)
  "Store \"org-bookmark:\" type link."
  (when (and (eq major-mode 'org-mode)
             (string-equal (buffer-name) (file-name-nondirectory org-bookmarks-file)))
    (let ((bookmark-title (substring-no-properties
                           (org-get-heading :no-tags :no-todo :no-priority :no-comment))))
      (org-link-store-props :type "org-bookmark"
                            :link (format "org-bookmark:%s" bookmark-title)
                            :description nil))))

(defun org-bookmarks-link-complete ()
  "Create a \"org-bookmark:\" type link using completion."
  (if-let* ((candidates org-bookmarks--candidates-cache)
            (minibuffer-allow-text-properties t)
            (completion-extra-properties
             (list :category 'org-bookmark
                   :annotation-function #'org-bookmarks--completion-annotator
                   :exit-function (lambda (string status)
                                    (message "[org-bookmarks] %s completion selected '%s'" status string))))
            (bookmark (completing-read "[org-bookmarks] Complete bookmark: "
                                       candidates nil 'require-match))
            (bookmark-title (get-text-property 0 'title bookmark)))
      (concat "org-bookmark:" bookmark-title)
    (user-error "The specified bookmark not found")))

(org-link-set-parameters "org-bookmark"
                         :follow #'org-bookmarks-link-open
                         :store #'org-bookmarks-link-store
                         :complete #'org-bookmarks-link-complete)

;;;###autoload
;;; Add `org-capture' template for adding new bookmark to `org-bookmarks-file'.
(defun org-bookmarks-add-org-capture-template ()
  "Add `org-capture' template for adding new bookmark to `org-bookmarks-file'."
  (require 'org-capture)
  ;; Delete existing key "b" binding in `org-capture-templates'.
  (if (and (assoc "b" org-capture-templates)
           (bound-and-true-p org-bookmarks-add-org-capture-template))
      (setq org-capture-templates
            (delete (assoc "b" org-capture-templates) org-capture-templates)))
  (add-to-list
   'org-capture-templates
   `("b" ,(format "%s\tAdd a new bookmark to %s"
                  (when (fboundp 'nerd-icons-mdicon)
                    (nerd-icons-mdicon "nf-md-bookmark_plus_outline" :face 'nerd-icons-blue))
                  (file-name-nondirectory org-bookmarks-file))
     entry (file ,(expand-file-name org-bookmarks-file))
     ,(concat "* %^{bookmark title}\t\t\t\t" (format ":%s:" org-bookmarks-tag) "
:PROPERTIES:
:URL:  %^C
:DATE: %t
:END:")
     :empty-lines 1
     :jump-to-captured t
     :refile-targets ((,org-bookmarks-file :maxlevel 3)))
   :append))



(provide 'org-bookmarks)

;;; org-bookmarks.el ends here
