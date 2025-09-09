;;; org-bookmarks.el --- Manage bookmarks in Org mode -*- lexical-binding: t; -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2024-2025 stardiviner <numbchild@gmail.com>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "29.1") (nerd-icons "0.1.0"))
;; Version: 1.3
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
;;   :hook (emacs-startup . org-bookmarks-add-to-org-capture-templates))

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
  :type '(repeat string)
  :safe #'listp
  :group 'org-bookmarks)

(defcustom org-bookmarks-browse-url-function #'browse-url
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

(defcustom org-bookmarks-db-auto-update-when-idle nil
  "Non-nil means auto update `org-bookmarks' database when Emacs idle."
  :type 'boolean
  :safe #'booleanp
  :group 'org-bookmarks)

(defcustom org-bookmarks-display-url t
  "Whether display URL of bookmark in candidate."
  :type 'boolean
  :safe #'booleanp
  :group 'org-bookmarks)

(defcustom org-bookmarks-display-screenshot nil
  "Whether display screenshot of bookmark in candidate."
  :type 'boolean
  :safe #'booleanp
  :group 'org-bookmarks)


;;; Helper Functions

(defun org-bookmarks--get-bookmark-content (bookmark-title)
  "Get the buffer content of bookmark entry with title BOOKMARK-TITLE."
  (if-let* ((file org-bookmarks-file)
            (buffer (or (get-buffer (file-name-nondirectory file))
                        (find-file-noselect file))))
      (with-current-buffer buffer
        (let ((marker (org-find-exact-headline-in-buffer bookmark-title buffer)))
          ;; (org-goto-marker-or-bmk marker) ; NOTE: it will open buffer and jump.
          (goto-char (marker-position marker)))
        (when (or (org-invisible-p) (org-invisible-p2))
          (org-fold-show-context))
        ;; (org-element-context)
        ;; reference from `org-get-entry'
        (save-excursion
          (org-back-to-heading t)
          (buffer-substring (point) (org-end-of-subtree t))))))

;;; TEST
;; (org-bookmarks--get-bookmark-content "xHamster")
;; (pp (org-bookmarks--get-bookmark-content "xHamster"))

(defun org-bookmarks--entry-screenshot (headline-element)
  "Return the bookmark HEADLINE object's webpage screenshot inline image."
  ;; limit in current headline entry.
  (let ((entry-begin (org-element-begin headline-element))
        (entry-end (org-element-end headline-element)))
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
        (when (and (string-equal link-type "file")
                   link-path
                   (member (intern (file-name-extension link-path)) image-types))
          (propertize " "
                      'display (create-image link-path nil nil
                                             :ascent 100
                                             ;; :height (* (default-font-height) 20)
                                             ;; :width 50
                                             )))))))

;; TEST:
;; (org-bookmarks--entry-screenshot (org-element-context))

(defun org-bookmarks--parse-element-as-candidate (headline-element)
  "Return candidate string from Org HEADLINE-ELEMENT."
  (when-let* ((tags (org-element-property :tags headline-element))
              ( (and (member org-bookmarks-tag tags)
                     (not (seq-intersection tags org-bookmarks-tag-exclude-list))))
              (url (alist-get "URL" (org-entry-properties headline-element 'standard) nil nil #'equal))
              (description (let* ((property-description (alist-get "DESCRIPTION" (org-entry-properties headline-element 'standard) "" nil #'equal)))
                             (string-fill property-description (or (- (window-width) 5) fill-column))))
              ;; bookmark extra info as bookmark completion candidate annotation.
              (screenshot (if org-bookmarks-display-screenshot
                              (org-bookmarks--entry-screenshot headline-element)
                            ""))
              (info (concat "\n" ; candidate display extra info in multi-line with "\n"
                            (when org-bookmarks-display-url
                              (concat "   " (propertize url 'face 'link) "\n")) ; property :URL:
                            (unless (string-empty-p description)
                              (concat "   " (propertize description 'face 'font-lock-comment-face) "\n")) ; property :DESCRIPTION:
                            ;; The screenshot inline image in bookmark entry body.
                            ;; FIXME: the screenshot inline image is not displaying.
                            (when (and org-bookmarks-display-screenshot screenshot)
                              (concat "   " screenshot "\n"))
                            "\n"))
              (headline-title (org-element-property :raw-value headline-element))
              (position (or (org-element-begin headline-element) (point))))
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
                  'title headline-title
                  'url url
                  'annotation info
                  ;; 'screenshot screenshot
                  'position position))))

;; TEST:
;; (org-bookmarks--parse-element-as-candidate (org-element-context))

(defun org-bookmarks--candidates (&optional file)
  "Return a list of candidates from FILE or default `org-bookmarks-file'."
  (let* ((file (or file org-bookmarks-file))
         (buffer (or (get-buffer (file-name-nondirectory file))
                     (find-file-noselect file))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (let ((candidates nil))
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (headline-element)
            (when-let* ((candidate (org-bookmarks--parse-element-as-candidate headline-element)))
              (push candidate candidates))))
        (nreverse candidates)))))

;; TEST:
;; (nth 80 (org-bookmarks--candidates org-bookmarks-file))

(defun org-bookmarks--completion-annotator (candidate)
  "Annotate bookmark completion CANDIDATE."
  (concat (propertize " " 'display '(space :align-to center))
          (get-text-property 0 'annotation candidate)))

(defun org-bookmarks--completion-action (candidate status)
  "The action function to be executed on selected completion CANDIDATE in STATUS."
  (message "[org-bookmarks] completion `%s' is selected with status %s" candidate status))

(defun org-bookmarks--return-candidates (&optional file)
  "Return `org-bookmarks' candidates which parsed from FILE."
  (if-let* ((file (or file org-bookmarks-file)))
      (org-bookmarks--candidates file)
    (user-error "File does not exist: %S" file)))

(defvar org-bookmarks--candidates-cache-alist nil
  "An cache alist variable of `org-bookmarks--candidates'.
It consists with (file-name-base . candidates-data).
The candidates-data is from function `org-bookmarks--return-candidates'.")

;; TEST:
;; (nth 88 (cddar org-bookmarks--candidates-cache-alist))

(defun org-bookmarks-db-cache-update (&optional file)
  "Update the `org-bookmarks' database cache for FILE."
  (interactive (list (completing-read "[org-bookmarks] update db cache from file: "
                                      (delq nil
                                            (list org-bookmarks-file
                                                  (when (derived-mode-p 'org-mode)
                                                    (buffer-file-name (current-buffer))))))))
  (let* ((file-absolute (expand-file-name file)))
    (add-to-list 'org-bookmarks--candidates-cache-alist
                 (cons file-absolute (org-bookmarks--return-candidates file-absolute))))
  (message "[org-bookmarks] database cache updated!"))

(defun org-bookmarks-db-cache-reset (&optional file)
  "Reset the `org-bookmarks' database cache for selected FILE.
Reset the whole database cache variable when none file select."
  (interactive (list (completing-read "[org-bookmarks] update db cache from file: "
                                      (list org-bookmarks-file
                                            (buffer-file-name (current-buffer))))))
  (if (string-empty-p file)
      (setf org-bookmarks--candidates-cache-alist nil)
    (setf (alist-get (expand-file-name file) org-bookmarks--candidates-cache-alist nil nil 'string-equal) nil)
    (message "[org-bookmarks] database reset for file <%s> records" file)))

;;; Auto update org-bookmarks database cache in Emacs idle timer.
(defcustom org-bookmarks-db-update-idle-interval (* 60 10)
  "The idle interval seconds for update `org-bookmarks' database cache."
  :type 'number
  :safe #'numberp
  :group 'org-bookmarks)

(when org-bookmarks-db-auto-update-when-idle
  (run-with-idle-timer org-bookmarks-db-update-idle-interval t 'org-bookmarks-db-cache-update))

;;;###autoload
(defun org-bookmarks (&optional file)
  "Open bookmark read from FILE or `org-bookmarks-file'."
  (interactive (list (expand-file-name
                      (completing-read "[org-bookmarks] Select bookmarks from file: "
                                       (delq nil
                                             (list org-bookmarks-file
                                                   (buffer-file-name (current-buffer))))))))
  (unless (alist-get file org-bookmarks--candidates-cache-alist nil nil 'equal)
    (org-bookmarks-db-cache-update file))
  (if-let* ((file (or file org-bookmarks-file))
            ( (file-exists-p file)))
      (if-let* ((candidates (alist-get file org-bookmarks--candidates-cache-alist nil nil 'equal))
                (minibuffer-allow-text-properties t)
                (completion-extra-properties
                 ;; Using the "bookmark" category caused the annotations to not show.
                 ;; I think that may have be do to vertico-mode, but it's
                 ;; probably worth using a unique category so users can exercise
                 ;; finer-grained customization.
                 (list :category 'org-bookmark
                       :annotation-function #'org-bookmarks--completion-annotator
                       :display-sort-function #'minibuffer-sort-by-history ; reference `vertico-sort-function'
                       :exit-function #'org-bookmarks--completion-action))
                (choice (completing-read "org-bookmarks: " candidates nil 'require-match))
                (url (get-text-property 0 'url choice)))
          (funcall org-bookmarks-browse-url-function url)
        (user-error "No bookmarks found in %S" file))
    (user-error "File does not exist: %S" file)))

(defun org-bookmarks-in-current-buffer ()
  "Open bookmark in current Org buffer."
  (interactive)
  (org-bookmarks (buffer-file-name (current-buffer))))

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
  (if-let* ((candidates (alist-get org-bookmarks-file org-bookmarks--candidates-cache-alist nil nil 'equal))
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

;;; Add `org-capture' template for adding new bookmark to `org-bookmarks-file'.

;;;###autoload
(defun org-bookmarks-add-to-org-capture-templates ()
  "Add `org-capture' template for adding new bookmark to `org-bookmarks-file'."
  (require 'org-capture)
  ;; Delete existing key "b" binding in `org-capture-templates'.
  (when (and (bound-and-true-p org-bookmarks-add-org-capture-template)
             (assoc "b" org-capture-templates))
    (setq org-capture-templates
          (remove (assoc "b" org-capture-templates) org-capture-templates)))
  
  (add-to-list
   'org-capture-templates
   `("b" ,(format "%s\tAdd a new bookmark to %s"
                  (when (fboundp 'nerd-icons-mdicon)
                    (nerd-icons-mdicon "nf-md-bookmark_plus_outline" :face 'nerd-icons-blue))
                  (file-name-nondirectory org-bookmarks-file))
     entry (file ,(expand-file-name org-bookmarks-file))
     ,(concat "* %^{bookmark title}\t\t\t\t" (format ":%s:" org-bookmarks-tag) "%^g" "
:PROPERTIES:
:URL:  %c
:DATE: %t
:DESCRIPTION: %^{DESCRIPTION}p
:END:")
     :empty-lines 1
     :jump-to-captured t
     :refile-targets ((,org-bookmarks-file :maxlevel 3)))
   :append)
  ;; Org capture template only available in `org-bookmarks-file' "Bookmarks.org".
  (add-to-list 'org-capture-templates-contexts `("b" ((in-buffer . ,(file-name-nondirectory org-bookmarks-file))))))




(provide 'org-bookmarks)

;;; org-bookmarks.el ends here
