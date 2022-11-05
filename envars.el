;;; envars.el --- Utilities for setting env variables -*- lexical-binding: t; -*-

;; Author: Chris Clark <cfclrk@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.17.0") (f "0.20.0") (s "1.12.0"))
;; URL: https://github.com/cfclrk/envars

;;; Commentary:

;; See documentation at https://github.com/cfclrk/envars

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

(require 'dash)
(require 'f)
(require 's)

;;; Options

(defgroup envars ()
  "Utilities to set and unset environment variables in Emacs."
  :group 'environment
  :prefix "envars-"
  :link '(url-link :tag "GitHub" "https://github.com/cfclrk/envars"))

(defcustom envars-dir (expand-file-name "~/")
  "Directory with env files."
  :group 'envars
  :type 'directory)

;;; Public functions

(defun envars-set-file (file-path)
  "Set or unset environment variables defined in FILE-PATH.

When used interactively, prompts for the file to load. The prompt
begins in `envars-dir'.

When used from elisp, FILE-PATH can either be absolute or
relative to `default-directory'.

The env file at FILE-PATH may make use of existing environment
variables, and tildes are expanded if they are the first
character of the value. However, other shellisms will not work."
  (interactive (list (read-file-name "ENV file: " envars-dir)))
  (let ((str (f-read-text file-path)))
    (envars-set-str str)))

(defun envars-unset-file (file-path)
  "Unset the environment variables definedd in FILE-PATH.

See the documentation for `envars-set-file'."
  (interactive (list (read-file-name "ENV file: " envars-dir)))
  (let ((str (f-read-text file-path)))
    (envars-unset-str str)))

(defun envars-set-str (str)
  "Set environment variables defined in string STR.

Parse STR like an env file. See the documentation for
`envars-set-file'."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (--map (s-split "=" it) lines)))
    (envars-set-pairs pairs)))

(defun envars-unset-str (str)
  "Unset environment variables defined in string STR.

Parse STR like an env file. See the documentation for
`envars-set-file'."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (--map (s-split "=" it) lines)))
    (envars-unset-pairs pairs)))

(defun envars-set-pairs (pairs)
  "Add PAIRS to `process-environment'.

PAIRS is a list of pairs, where each pair is an environment
variable name and value."
  (-each pairs #'envars--export-pair))

(defun envars-unset-pairs (pairs)
  "Remove PAIRS from `process-environment'.

PAIRS is a list of pairs, where each pair is an environment
variable name and value.

The key of each pair is the environment variable name. The value
of each pair is discarded, as the environment variable will be
unset regardless of its value."
  (envars--unset-names (-map 'car pairs)))

;;; Private functions

(defun envars--export-pair (pair)
  "Set an environment variable PAIR.
PAIR is a list of size 2, where first element is an environment
variable name and the second element is the value.

If the second element begins with a ~, it is treated as a file
path and expanded.

If the second element begins with 'nosubst:', the value is
treated as a literal string, and no variable interpolation is
performed."
  (let* ((name (car pair))
         (val (car (cdr pair)))

         ;; If the value of the pair is an number, convert it to a string
         (string_val (if (numberp val)
                         (number-to-string val)
                       val))

         ;; If the value starts with ~, expand it like a path
         (full_val (if (string-prefix-p "~" string_val)
                       (expand-file-name string_val)
                     string_val)))

    ;; If the value starts with "nosubst:", do not do variable interpolation
    (if (string-prefix-p "nosubst:" full_val)
        (setenv name (s-chop-prefix "nosubst:" full_val))
      (setenv name full_val t))))

(defun envars--unset-names (names)
  "Remove NAMES from `process-environment'.
NAMES is a list of environment variable names which may or may
not be currently set. This function removes each given name from
`process-environment' if it is set."
  (-each names #'envars--unset-name))

(defun envars--unset-name (name)
  "Unset the environment variable NAME.
Unset environment variable NAME by removing it from
`process-environment' if it is there.

Note: calling `setenv' with a prefix argument sets the variable's
value to nil, but the variable remains in `process-environment'.
This function completely removes the variable from
`process-environment'."
  (let* ((name (if (multibyte-string-p name)
                   (encode-coding-string name locale-coding-system t)
                 name))
         (index (-elem-index name (envars--get-names))))
    (if index
        (setq process-environment (-remove-at index process-environment))
      process-environment)))

(defun envars--get-names ()
  "Return names of all current environment variables."
  (--map (car (s-split "=" it)) process-environment))

(provide 'envars)
;;; envars.el ends here
