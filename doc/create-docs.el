;;; create-docs.el --- Create package docs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides functions to generate a README.md, package texinfo documentation,
;; and package Commentary from the doc.org file.

;;; Code:

(require 'f)
(require 'ox-extra)
(require 'ox-gfm)
(require 'ox-texinfo)
(require 'projectile)
(require 'markdown-mode)
(require 'whitespace)

(defun proj-file (rel-path)
  "Return the absolute path to REL-PATH.
REL-PATH is a path relative to this project root."
  (f-join (projectile-project-root) rel-path))

(defun create-docs ()
  "Create the package README.md, texinfo, and commentary."
  (let ((make-backup-files nil))
    (create-readme)
    (create-texinfo)
    (create-commentary)))

(defun create-texinfo ()
  "Create the package texinfo docs.
This exports sections tagged :info: in doc.org to
texinfo. This creates two new files: a .texi file and a .info
file."
  (with-temp-buffer
    (insert-file-contents (proj-file "doc/doc.org"))
    (let ((org-export-select-tags '("info"))
          (org-export-with-tags nil))
      (ox-extras-activate '(ignore-headlines))
      (org-export-to-file 'texinfo (proj-file "doc/envars.texi")
        nil nil nil nil nil (lambda (file) (org-texinfo-compile file))))))

(defun create-readme ()
  "Create the README.md.
This exports sections tagged :readme: in doc.org to a
README.md."
  ;; Run an org export to github-flavored-markdown
  (with-temp-buffer
    (insert-file-contents (proj-file "doc/doc.org"))
    (let ((org-export-select-tags '("readme"))
          (org-export-with-tags nil))
      (org-export-to-file 'gfm (proj-file "README.md"))))

  ;; Format README.md
  (with-temp-buffer
    (insert-file-contents (proj-file "README.md"))
    (markdown-mode)
    (let ((fill-column 80))
      (fill-region (point-min) (point-max)))
    (write-file (proj-file "README.md"))))

(defun create-commentary ()
  "Create the Commentary section in envars.el.
This exports sections tagged :commentary: in doc.org to
markdown, then puts the markdown in the Commentary section of
envars.el."
  ;; Run an org export to markdown
  (with-temp-buffer
    (insert-file-contents (proj-file "doc/doc.org"))
    (let ((org-export-select-tags '("commentary"))
          (org-export-with-tags nil)
          (org-export-show-temporary-export-buffer nil))
      (ox-extras-activate '(ignore-headlines))
      (org-export-to-buffer 'md "*Org MD Export*")))

  ;; Format the markdown
  (with-current-buffer "*Org MD Export*"
    (markdown-mode)
    (let ((fill-column 77))
      (fill-region (point-min) (point-max)))

    ;; Remove extra leading or trailing newlines
    (let ((whitespace-style '(empty)))
      (whitespace-cleanup))

    ;; Prepend a ";; " to every line
    (string-insert-rectangle (point-min) (point-max) ";; ")

    ;; Remove extra trailing spcaces
    (let ((whitespace-style '(trailing)))
      (whitespace-cleanup)))

  (with-current-buffer "*Org MD Export*"
    whitespace-style)

  ;; Replace the Commentary section with the newly created markdown
  (with-temp-buffer
    (insert-file-contents (proj-file "envars.el"))
    (let ((beg (search-forward ";;; Commentary:\n"))
          (end (- (search-forward ";;; Code:") 10)))
      (delete-region beg end)
      (goto-char beg)
      (insert "\n")
      (insert-buffer-substring "*Org MD Export*")
      (write-file (proj-file "envars.el")))))

(provide 'create-docs)
;;; create-docs.el ends here
