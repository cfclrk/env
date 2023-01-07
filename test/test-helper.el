;;; test-helper.el --- Helpers for source-env-file-test.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Ert-runner evaluates this file before running tests.

;;; Code:

(require 'dash)
(require 'f)
(require 'projectile)

;;; Functions

(defun proj-file (rel-path)
  "Return the absolute path to REL-PATH.
REL-PATH is a path relative to this project root."
  (f-join (projectile-project-root) rel-path))

;;; Test fixtures

(defmacro with-process-environment (temp-environment &rest forms)
  "Set `process-environment' to TEMP-ENVIRONMENT and evaluate FORMS.
After evaluating FORMS, the original `process-environment' is restored."
  (declare (indent 1))
  `(let ((orig-process-environment process-environment))
    (unwind-protect
        (progn
          (setq process-environment ,temp-environment)
          ,@forms)
      (setq process-environment orig-process-environment))))

;;; test-helper.el ends here
