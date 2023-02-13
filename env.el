;;; env.el --- Utilities for setting env variables -*- lexical-binding: t; -*-

;; Author: Chris Clark <cfclrk@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.17.0") (f "0.20.0") (s "1.12.0"))
;; URL: https://github.com/cfclrk/env

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

;;; Commentary:

;; See the README.md in this package, which is also visible at:
;; https://github.com/cfclrk/env

;;; Code:

(require 'dash)
(require 'f)
(require 's)

;;; Options

(defgroup env ()
  "Utilities to set and unset environment variables in Emacs."
  :group 'environment
  :prefix "env-"
  :link '(url-link :tag "GitHub" "https://github.com/cfclrk/env"))

(defconst env-project-dir
  (file-name-directory (if load-in-progress
                           load-file-name
                         (buffer-file-name)))
  "The directory where this project's source code is located.")

(defcustom env-dir (expand-file-name "~/")
  "Directory with env files."
  :group 'env
  :type 'directory)

(defcustom env-pre-eval-hook nil
  "List of functions to run before subshell evaluation."
  :group 'env
  :type 'hook)

(defcustom env-post-eval-hook nil
  "List of functions to run after subshell evaluation."
  :group 'env
  :type 'hook)

;;; Set and unset functions

;;;###autoload
(defun env-set-file (file-path)
  "Set environment variables defined in the file at FILE-PATH.

When used interactively, prompts for the file to load. The prompt
begins in `env-dir'. When used from elisp, FILE-PATH can
either be absolute or relative to `default-directory'.

The env file at FILE-PATH should be in the standard env file
format."
  (interactive (list (read-file-name "ENV file: " env-dir)))
  (env-set-str (f-read-text file-path)))

;;;###autoload
(defun env-unset-file (file-path)
  "Unset the environment variables defined in FILE-PATH.

See the documentation for `env-set-file'."
  (interactive (list (read-file-name "ENV file: " env-dir)))
  (env-unset-str (f-read-text file-path)))

(defun env-set-str (str)
  "Set environment variables defined in the given string STR.

Parse STR like an env file. STR is split into newline-delimited
lines, where each line is a key/value pair."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (env--lines-to-pairs lines)))
    (env-set-pairs pairs)))

(defun env-unset-str (str)
  "Unset environment variables defined in string STR.

Parse STR like an env file. STR is split into newline-delimited
pairs, where the key of each pair is the environment variable
name. The value of each pair is discarded, as the environment
variable will be unset regardless of its value."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (env--lines-to-pairs lines)))
    (env-unset-pairs pairs)))

(defun env-set-pairs (pairs)
  "Set the environment variables defined in the given PAIRS.

PAIRS is a list of pairs, where each pair is an environment
variable name and value."
  (-> pairs
      (env--eval-pairs)
      (-each #'env--set-pair)))

(defun env-unset-pairs (pairs)
  "Unset the environment variables defined in the given PAIRS.

PAIRS is a list of pairs, where each pair is an environment
variable name and value. The value in each pair doesn't matter;
each environment variable will be unset regardless of its value."
  (env-unset-names (-map 'car pairs)))

(defun env-unset-names (names)
  "Unset environment variables with the given NAMES.

NAMES is a list of environment variable names which may or may
not be currently set. This function removes each name from
`process-environment' if it is set."
  (-each names #'env-unset-name))

;;;###autoload
(defun env-unset-name (name)
  "Unset the environment variable NAME.

Unset the given environment variable by removing it from
`process-environment' if it is there. Note that calling `setenv'
with a prefix argument can 'unset' a variable by setting its
value to nil, but the variable remains in `process-environment'.
This function completely removes the variable from
`process-environment'.

Neither Emacs nor bash directly support non-ASCII characters as
environment variables (see [The Open Group][1]), but Emacs can
fake it by using escaped sequences of unicode code points.

[1]: https://pubs.opengroup.org/onlinepubs/9699919799/"
  (interactive (list (completing-read "" (env--get-names))))
  (let* ((encoded-name (if (multibyte-string-p name)
                           (encode-coding-string name locale-coding-system t)
                         name))
         (index (-elem-index encoded-name (env--get-names))))
    (if index
        (setq process-environment
              (-remove-at index process-environment))
      process-environment)))

;;; Post-eval filters

;;; Private functions

(defun env--set-pair (pair)
  "Set an environment variable PAIR."
  (let ((name (car pair))
        (val (car (cdr pair))))
    (setenv name val)))

(defun env--get-names ()
  "Return names of all current environment variables."
  (--map (car (s-split "=" it)) process-environment))

;;;; Conversion functions

(defun env--pairs-to-script (pairs)
  "Turn PAIRS into a sh script."
  (->> pairs
       (--map (s-join "=" it))
       (env--lines-to-script)))

(defun env--lines-to-script (lines)
  "Turn LINES into a sh script."
  (->> lines
      (--map (s-prepend "export " it))
      (s-join "\n")))

(defun env--lines-to-pairs (lines)
  "Return a list of pairs of LINES."
  (--map (s-split "=" it) lines))

;;;; Eval functions

(defun env--eval-pairs (pairs)
  "Eval PAIRS.

- Capture the current process environment
- Run pre-eval hooks
- Evaluate given pairs in a subshell
- Run post-eval hooks

The result is diffed against the captured process environment.

Returns the list of pairs of environment variables that should be
set in the new environment."
  (let* (;; Capture the current environment
         (old-pairs (env--lines-to-pairs process-environment))

         ;; TODO: Run the pre-eval hooks

         ;; Evaluate given pairs in a subshell
         (stdout (env--eval-script
                  (env--pairs-to-script pairs)))

         ;; Convert the result into pairs
         (out-pairs (-> stdout
                        s-trim
                        s-lines
                        env--lines-to-pairs))

         ;; TODO: Run the post-eval hooks. Right now this is just the
         ;; remove-sh-vars function.
         (new-pairs (env-remove-sh-vars out-pairs)))

    ;; And return the difference!
    (-difference new-pairs old-pairs)))

(defun env--eval-script (script)
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

(defun env-remove-sh-vars (pairs)
  "Remove some from PAIRS.

The sh shell initializes these environment varibales.

This is the default post-eval filter."
  (let ((ignored-env-vars '("DISPLAY"
                            "PWD"
                            "SHLVL"
                            "_")))
    (-filter
     (lambda (pair) (not (member (car pair) ignored-env-vars)))
     pairs)))

(provide 'env)
;;; env.el ends here
