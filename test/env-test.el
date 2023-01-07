;;; env-test.el --- Tests for package env  -*- lexical-binding: t;

;;; Commentary:

;; Tests for package env.

;;; Code:

(require 'dash)
(require 'env)
(require 'f)
(require 'projectile)

;;; Test helper functions

(defun proj-file (rel-path)
  "Return the absolute path to REL-PATH.
REL-PATH is a path relative to this project root."
  (f-join (projectile-project-root) rel-path))

;; TODO: Add this test to show that every env var that the sh shell adds is
;; ignored by the default filter.

(ert-deftest env-set-file ()
  "Test running `env-set-file'."
  (let ((process-environment '())
        (test-file (proj-file "test/examples/simple")))

    (env-set-file test-file)

    (should (equal "foo"
                   (getenv "A")))
    (should (equal "bar"
                   (getenv "B")))
    (should (equal "R$%!$KP$"
                   (getenv "C")))
    (should (equal "foo-bar"
                   (getenv "D")))
    (should (equal (expand-file-name "~/cats")
                   (getenv "E")))))

(ert-deftest env-unset-file ()
  "Test running `env-unset-file'."
  (let ((process-environment '("A=a" "B=b" "C=C" "Z=z"))
        (test-file (proj-file "test/examples/simple")))

    (env-unset-file test-file)

    (should (equal '("Z=z")
                   process-environment))))

(ert-deftest env-set-str ()
  "Test running `env-set-str'."
  (let ((process-environment '())
        (test-str "A=a\nB=b"))

    (env-set-str test-str)

    (should (equal "a" (getenv "A")))
    (should (equal "b" (getenv "B")))))

(ert-deftest env-unset-str ()
  "Test running `env-unset-str'."
  (let ((process-environment '("FOO=foo" "BAR=bar" "CATS=cats"))
        (test-str "FOO=foo\nBAR=bar"))

    (env-unset-str test-str)

    (should (-same-items? '("CATS=cats")
                          process-environment))))

(ert-deftest env-set-pairs ()
  "Test running `env-set-pairs' to set env vars."
  (let ((process-environment '()))

    (env-set-pairs '(("A" "a")
                     ("B" "'R$%!$KP$'")))

    (should (-same-items? '("B=R$%!$KP$" "A=a")
                          process-environment))))

(ert-deftest env-unset-pairs ()
  "Test running `env-unset-pairs' to unset env vars."
  (let ((process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")))

    (env-unset-pairs '(("FOO" "foo")
                       ("BAR" "barrr")))

    (should (-same-items? '("BAZ=baz")
                          process-environment))))

(ert-deftest env-unset-names ()
  "Test running `env-unset-names'."
  (let ((process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")))

    (env-unset-names '("FOO" "BAR"))

    (should (equal nil (getenv "FOO")))
    (should (equal nil (getenv "BAR")))
    (should (equal "baz" (getenv "BAZ")))
    (should (equal '("BAZ=baz") process-environment))))

;; (ert-deftest env--unset-names/non-existent-name ()
;;   "Test running `env--unset-names' to unset non-existent env var.
;; This shouldn't cause a problem. The environment should remain
;; unchanged."
;;   (with-process-environment '("FOO=foo")
;;     (env--unset-names '(("BAR" "bar")))
;;     (should (equal nil (getenv "BAR")))
;;     (should (equal '("FOO=foo") process-environment))))

;; (ert-deftest env--export-pair ()
;;   "Test running `env--export-pair' to set a single environment variable."
;;   (with-process-environment '()
;;     (env--export-pair '("FOO" "foo"))
;;     (should (equal "foo" (getenv "FOO")))

;;     (env--export-pair '("FOO" 1))
;;     (should (equal "1" (getenv "FOO")))))

;; (ert-deftest env--unset-name/simple ()
;;   "Test running `env--unset-name' to unset a single,
;; simple environment variable."
;;   (with-process-environment '("CATS=cats")
;;     (should (equal "cats" (getenv "CATS")))
;;     (env--unset-name "CATS")
;;     (should (equal nil (getenv "CATS")))))

(ert-deftest env--eval-pairs ()
  "Test running `env--eval-pairs'."
  (should (equal '(("FOO" "foo")
                   ("BAR" "bar"))
                 (env--eval-pairs '(("FOO" "foo")
                                    ("BAR" "bar")))))

  ;; Should be able to interpolate values
  (should (equal '(("FOO" "foo")
                   ("BAR" "foo-bar"))
                 (env--eval-pairs '(("FOO" "foo")
                                    ("BAR" "$FOO-bar")))))

  ;; Should be able to surround a value in single quotes to make it a literal
  ;; value
  (should (-same-items? '(("FOO" "f$oo")
                          ("B" "R$%!$KP$"))
                        (env--eval-pairs '(("FOO" "'f$oo'")
                                           ("B" "'R$%!$KP$'"))))))

;;; env-test.el ends here
