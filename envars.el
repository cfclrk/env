;;; envars.el --- Utilities for setting env variables -*- lexical-binding: t; -*-

;; Package-Requires: ((dash "2.17.0") (f "0.20.0") (s "1.12.0"))
;; Package-Version: 0.0.1

;;; Commentary:

;; Set or unset environment variables from an "env" or dotenv file.
;;
;; This package provides two interactive functions:
;;
;; 1.  `envars`: Set all the environment variables defined in an env file.
;; 2.  `envars-unset`: Unset all the environment variables defined in an env
;;     file.
;;
;; When used interactively, each function prompts for a file. By default, the
;; prompt begins at `envars-dir`.
;;
;;
;; # Usage
;;
;; Start by creating an env file in `envars-dir` (by default, `~/.env/`).  For
;; example, create this file in `~/.env/foo`:
;;
;;     FOO=~/foo
;;     BAR=$FOO/bar
;;     ОФИС=ДОМ
;;     BAZ=nosubst:FOO$BAR
;;
;; Now, you can run:
;;
;; -   `M-x envars`, which will prompt you for a file. All the environment
;;     variables defined in the file will be **set**.
;; -   `M-x envars-unset`, which will prompt you for a file. All the environment
;;     variables defined in the file will be **unset**.
;;
;;
;; ## Usage from Elisp
;;
;; To set env variables defined in `~/.env/foo`:
;;
;;     (envars (expand-file-name "~/.env/foo"))
;;
;; Or, if you have a string instead of a file:
;;
;;     (envars-str "FOO=foo\nBAR=bar")
;;
;;
;; ## Usage from org-mode
;;
;; The example below shows a convenient way to declare and set environment
;; variables in an `org` document:
;;
;;     #+NAME: env
;;     | Var  | Value           |
;;     |------+-----------------|
;;     | FOO  | ~/foo           |
;;     | BAR  | $FOO/bar        |
;;     | ОФИС | ДОМ             |
;;     | BAZ  | nosubst:FOO$BAR |
;;
;;     #+begin_src emacs-lisp :var env=env
;;       (envars-set-pairs env)
;;     #+end_src
;;
;;
;; # File Format
;;
;; Each line in the file should be in a `KEY=VALUE` format, with one entry per
;; line. This package does not invoke a shell to interpret the file, so most
;; shell-isms will not work. However, the env file may:
;;
;; -   Use existing environment variables
;; -   Define an environment variable and use it in successive lines
;; -   A `~` is expanded if it is the first character in the value
;; -   If a value starts with `nosubst:`, no variable substitution will be
;;     performed. You need this if there is a literal `$` in the value.
;;
;;; Code:

(require 'dash)
(require 'f)
(require 's)

;;; Options

(defgroup envars nil
  "Utilities to set and unset environment variables in Emacs."
  :group 'environment
  :prefix "envars-"
  :link '(url-link :tag "GitHub" "https://github.com/cfclrk/envars"))

(defcustom envars-dir (expand-file-name "~/")
  "Directory with env files."
  :group 'envars
  :type 'file)

;;; Public

(defun envars (file-path)
  "Set or unset environment variables from file FILE-PATH.

When used interactively, `envars' prompts for the file
to load, defaulting to the directory `source-env-dir'.

The env file FILE-PATH may make use of existing environment
variables, and tildes are expanded if they are the first
character of the value. However, other shell-isms will not work.

Prefixed with one \\[universal-argument], unset the environment
variables defined in file F."
  (interactive (list (read-file-name "ENV file: " envars-dir)))
  (let ((str (f-read-text file-path)))
    (if current-prefix-arg
        (envars-unset-str str)
      (envars-str str))))

(defun envars-unset (file-path)
  "Unset environment variables from file FILE-PATH.

See the documentation for `envars'."
  (interactive (list (read-file-name "ENV file: " envars-dir)))
  (let ((str (f-read-text file-path)))
    (envars-unset-str str)))

(defun envars-str (str)
  "Set environment variables from string STR.

Parse STR as an env file. See the documentation for
`envars'."
  (let* ((lines (s-lines (s-trim str)))
         (pairs (--map (s-split "=" it) lines)))
    (envars-set-pairs pairs)))

(defun envars-unset-str (str)
  "Unset environment variables from string STR.

Parse STR as an env file. See the documentation for
`envars'."
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

;;; Private

(defun envars--export-pair (pair)
  "Set an environment variable PAIR.
PAIR is a list of size 2, where first element is an environment
variable name and the second element is the value.

If the second element begins with a ~, it is treated as a file
path and expanded.

If the second element begins with nosubst:, it is treated as a
literal string, and no variable interpolation is performed."
  (let* ((name (car pair))
         (val (car (cdr pair)))

         ;; if the value of the pair is an number, convert it to a string
         (string_val (if (numberp val)
                         (number-to-string val)
                       val))

         ;; if the value starts with ~, expand it like a path
         (full_val (if (string-prefix-p "~" string_val)
                       (expand-file-name string_val)
                     string_val)))

    ;; if the value starts with "nosubst:", do not do variable interpolation
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
value to nil, but the variable is still present. This function
completely removes the variable from `process-environment'."
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
