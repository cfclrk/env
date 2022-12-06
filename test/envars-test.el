;;; envars-test.el --- Tests for package envars  -*- lexical-binding: t;

;;; Commentary:

;; Tests for package envars.

;; Note: the `with-process-environment' macro is defined in test-helper.el.

;;; Code:

(require envars)

;; TODO: Add this test to show that every env var that the sh shell adds is
;; ignored by the default filter.

(ert-deftest envars-set-file ()
  "Test running `envars-set-file'."
  (let ((process-environment '())
        (test-file (proj-file "test/examples/variables")))

    ;; Sets FOO, BAR, and BAZ
    (envars-set-file test-file)

    (should (equal "foo"
                   (getenv "FOO")))
    (should (equal "foo-bar"
                   (getenv "BAR")))
    (should (equal (expand-file-name "~/cats")
                   (getenv "BAZ")))))

(ert-deftest envars-unset-file ()
  "Test running `envars-unset-file'.

This should unset the env vars defined in the file."
  (let ((process-environment '("FOO=foo" "BAR=bar" "CATS=cats"))
        (test-file (proj-file "test/examples/simple")))

    ;; Unsets FOO, BAR, and BAZ
    (envars-unset-file test-file)

    (should (equal '("CATS=cats")
                   process-environment))))

(ert-deftest envars-set-str ()
  "Test running `envars-set-str'."
  (let ((process-environment '())
        (test-str "FOO=foo\nBAR=bar"))

    (envars-set-str test-str)

    (should (equal "foo"
                   (getenv "FOO")))
    (should (equal "bar"
                   (getenv "BAR")))))

(ert-deftest envars-unset-str ()
  "Test running `envars-set-str'."
  (let ((process-environment '("FOO=foo" "BAR=bar" "CATS=cats"))
        (test-str "FOO=foo\nBAR=bar"))

    (envars-unset-str test-str)

    (should
     (-same-items? '("CATS=cats")
                   process-environment))))

(ert-deftest envars-set-pairs ()
  "Test running `envars-set-pairs' to set env vars."
  (with-process-environment '()
    (envars-set-pairs '(("A" "a")
                        ("B" "'R$%!$KP$'")))
    (should
     (-same-items? '("B=R$%!$KP$" "A=a")
                   process-environment))))

(ert-deftest envars-set-pairs ()
  "Test running `envars-set-pairs' to set env vars."
  (let ((process-environment '()))
    (envars-set-pairs '(("A" "a")
                        ("B" "'R$%!$KP$'")))
    (should
     (-same-items? '("B=R$%!$KP$" "A=a")
                   process-environment))))

(ert-deftest envars-unset-pairs ()
  "Test running `envars-unset-pairs' to unset env vars."
  (let ((process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")))

    (envars-unset-pairs '(("FOO" "foo") ("BAR" "barrr")))

    (should
     (-same-items? '("BAZ=baz")
                   process-environment))))

;; (ert-deftest envars--unset-names/simple ()
;;   "Test running `envars-unset-names' to unset env vars."
;;   (with-process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")
;;     (envars--unset-names '("FOO" "BAR"))
;;     (should (equal nil (getenv "FOO")))
;;     (should (equal nil (getenv "BAR")))
;;     (should (equal "baz" (getenv "BAZ")))
;;     (should (equal '("BAZ=baz") process-environment))))

;; (ert-deftest envars--unset-names/non-existent-name ()
;;   "Test running `envars--unset-names' to unset non-existent env var.
;; This shouldn't cause a problem. The environment should remain
;; unchanged."
;;   (with-process-environment '("FOO=foo")
;;     (envars--unset-names '(("BAR" "bar")))
;;     (should (equal nil (getenv "BAR")))
;;     (should (equal '("FOO=foo") process-environment))))

;; (ert-deftest envars--export-pair ()
;;   "Test running `envars--export-pair' to set a single environment variable."
;;   (with-process-environment '()
;;     (envars--export-pair '("FOO" "foo"))
;;     (should (equal "foo" (getenv "FOO")))

;;     (envars--export-pair '("FOO" 1))
;;     (should (equal "1" (getenv "FOO")))))

;; (ert-deftest envars--unset-name/simple ()
;;   "Test running `envars--unset-name' to unset a single,
;; simple environment variable."
;;   (with-process-environment '("CATS=cats")
;;     (should (equal "cats" (getenv "CATS")))
;;     (envars--unset-name "CATS")
;;     (should (equal nil (getenv "CATS")))))

(ert-deftest envars--eval-pairs ()
    "Test running `envars--eval-pairs'."
    (should
     (equal
      '(("FOO" "foo")
        ("BAR" "bar"))
      (envars--eval-pairs '(("FOO" "foo")
                            ("BAR" "bar")))))

    (should
     (equal
      '(("FOO" "foo")
        ("BAR" "foo-bar"))
      (envars--eval-pairs '(("FOO" "foo")
                            ("BAR" "$FOO-bar")))))

    ;; surround a value in single quotes to make it a literal value
    (should
     (-same-items?
      '(("FOO" "f$oo")
        ("B" "R$%!$KP$"))
      (envars--eval-pairs '(("FOO" "'f$oo'")
                            ("B" "'R$%!$KP$'"))))))

;;; envars-test.el ends here
