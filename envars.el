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

;; TODO: name: environ?
;;
;;

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

;;; Set and unset functions

(defun envars-set-file (file-path)
  "Set or unset environment variables defined in FILE-PATH.

When used interactively, prompts for the file to load. The prompt
begins in `envars-dir'.

When used from elisp, FILE-PATH can either be absolute or
relative to `default-directory'.

The env file at FILE-PATH should be in the standard env file
format."
  (interactive (list (read-file-name "ENV file: " envars-dir)))
  (envars-set-str (f-read-text file-path)))

(defun envars-unset-file (file-path)
  "Unset the environment variables defined in FILE-PATH.

See the documentation for `envars-set-file'."
  (interactive (list (read-file-name "ENV file: " envars-dir)))
  (envars-unset-str (f-read-text file-path)))

(defun envars-set-str (str)
  "Set environment variables defined in string STR.

Parse STR like an env file. STR is split into newline-delimited
lines, where each line is a key/value pair."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (envars--lines-to-pairs lines)))
    (envars-set-pairs pairs)))

(defun envars-unset-str (str)
  "Unset environment variables defined in string STR.

Parse STR like an env file. STR is split into newline-delimited
pairs, where the key of each pair is the environment variable
name. The value of each pair is discarded, as the environment
variable will be unset regardless of its value."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (envars--lines-to-pairs lines)))
    (envars-unset-pairs pairs)))

(defun envars-set-pairs (pairs)
  "Add PAIRS to `process-environment'.

PAIRS is a list of pairs, where each pair is an environment
variable name and value."
  (-> pairs
      (envars--eval-pairs)
      (-each #'envars--export-pair)))

(defun envars-unset-pairs (pairs)
  "Remove PAIRS from `process-environment'.

PAIRS is a list of pairs, where each pair is an environment
variable name and value. The value in each pair doesn't matter;
each environment variable will be unset regardless of its value."
  (envars--unset-names (-map 'car pairs)))

;;; Private functions

;;; Post-eval filters

(defun envars-remove-sh-vars (pairs)
  "Remove some from PAIRS.

The sh shell initializes these environment varibales. See the
corresponding test to verify this.

This is the default post-eval filter."
  (let ((ignored-env-vars '("DISPLAY"
                            "PWD"
                            "SHLVL"
                            "_")))
    (-filter
     (lambda (pair) (not (member (car pair) ignored-env-vars)))
     pairs)))

(defun envars--export-pair (pair)
  "Set an environment variable PAIR."
  (let ((name (car pair))
        (val (car (cdr pair))))
    (setenv name val)))

(defun envars--unset-names (names)
  "Remove NAMES from `process-environment'.

NAMES is a list of environment variable names which may or may
not be currently set. This function removes each given name from
`process-environment' if it is set."
  (-each names #'envars--unset-name))

(defun envars--unset-name (name)
  "Unset the environment variable NAME.

Unset the given environment variable by removing it from
`process-environment' if it is there. Note that calling `setenv'
with a prefix argument can 'unset' a variable by setting its
value to nil, but the variable remains in `process-environment'.
This function completely removes the variable from
`process-environment'.

Neither Emacs nor bash directly support non-ASCII characters as
environment variables[1], but Emacs can fake it by using escaped
sequences of unicode code points.

[1]: https://pubs.opengroup.org/onlinepubs/9699919799/"
  (let* ((encoded-name (if (multibyte-string-p name)
                           (encode-coding-string name locale-coding-system t)
                         name))
         (index (-elem-index encoded-name (envars--get-names))))
    (if index
        (setq process-environment
              (-remove-at index process-environment))
      process-environment)))

(defun envars--get-names ()
  "Return names of all current environment variables."
  (--map (car (s-split "=" it)) process-environment))

;;;; Conversion functions

(defun envars--pairs-to-script (pairs)
  "Turn PAIRS into a sh script."
  (->> pairs
       (--map (s-join "=" it))
       (envars--lines-to-script)))

(defun envars--lines-to-script (lines)
  "Turn LINES into a sh script."
  (->> lines
      (--map (s-prepend "export " it))
      (s-join "\n")))

(defun envars--lines-to-pairs (lines)
  "Return a list of pairs of LINES."
  (--map (s-split "=" it) lines))

;;;; Eval functions

(defun envars--eval-pairs (pairs)
  "Eval PAIRS.

- Capture the current process environment
- Run pre-eval hooks
- Evaluate given pairs in a subshell
- Run post-eval hooks

The result is diffed against the captured process environment.

Returns the list of pairs of environment variables that should be
set in the new environment."
  (let* (;; Capture the current environment
         (old-pairs (envars--lines-to-pairs process-environment))

         ;; Run the pre-eval hooks
         ;; TODO

         ;; Evaluate given pairs in a subshell
         (stdout (envars--eval-script
                  (envars--pairs-to-script pairs)))

         ;; Convert the result into pairs
         (out-pairs (-> stdout
                        s-trim
                        s-lines
                        envars--lines-to-pairs))

         ;; Run the post-eval hooks
         ;; TODO
         (new-pairs (envars-remove-sh-vars out-pairs)))

    ;; And return the difference!
    (-difference new-pairs old-pairs)))

(defun envars--eval-script (script)
  "Start a subprocess, execute SCRIPT, and return the resulting env.

SCRIPT can be any sh script. This function appends the 'env'
command to the end of the script, and then returns stdout."
  (with-temp-buffer
    (let* ((env-script (s-append "\nenv" script))
           (ret-code (call-process "sh"
                                   nil t nil
                                   shell-command-switch
                                   env-script)))
      (buffer-string))))

(provide 'envars)
;;; envars.el ends here
