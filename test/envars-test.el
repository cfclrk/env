;;; envars-test.el --- Tests for package envars  -*- lexical-binding: t;

;;; Commentary:

;; Tests for package envars.

;; Note: the `with-process-environment' macro is defined in test-helper.el.

;;; Code:

(ert-deftest envars/simple ()
  "Test running `envars' to set env vars."
  (with-process-environment '()
    (let ((test-file (proj-file "test/examples/variables")))
      (envars test-file)
      (should (equal "foo" (getenv "FOO")))
      (should (equal "foo-bar" (getenv "BAR")))
      (should (equal (expand-file-name "~/cats") (getenv "BAZ"))))))

(ert-deftest envars/with-prefix-arg ()
  "Test running `envars' with a prefix arg to unset simple
env vars."
  (with-process-environment '("FOO=foo" "BAR=bar")
    (let ((test-file (proj-file "test/examples/simple"))
          (current-prefix-arg 4))
      (envars test-file)
      (should (equal nil (getenv "FOO")))
      (should (equal nil (getenv "BAR")))
      (should (equal '() process-environment)))))

(ert-deftest envars/multibyte ()
  "Test running `envars' to set env vars with multibyte characters."
  (with-process-environment '()
    (should (equal nil (getenv "A")))
    (let ((test-file (proj-file "test/examples/multibyte")))
      (envars test-file)
      (should (equal "Д" (getenv "Ф")))
      (should (equal "µ" (getenv "¥")))
      (should (equal '("\302\245=\302\265" "\320\244=\320\224")
                     process-environment)))))

(ert-deftest envars/multibyte-with-prefix-arg ()
  "Test running `envars' with a prefix arg to unset
multibyte env vars."
  (with-process-environment
      '("\302\245=\302\265" "\320\244=\320\224")
    (should (equal "Д" (getenv "Ф")))
    (should (equal "µ" (getenv "¥")))
    (let ((test-file (proj-file "test/examples/multibyte"))
          (current-prefix-arg 4))
      (envars test-file)
      (should (equal nil (getenv "Ф")))
      (should (equal nil (getenv "¥"))))))

(ert-deftest envars-set-pairs ()
  "Test running `envars-set-pairs' to set env vars."
  (with-process-environment '()
    (envars-set-pairs '(("A" "a")
                                ("B" "nosubst:R$%!$KP$")))
    (should (equal "a" (getenv "A")))
    (should (equal "R$%!$KP$" (getenv "B")))
    (should (equal '("B=R$%!$KP$" "A=a") process-environment))))

(ert-deftest envars-unset-pairs ()
  "Test running `envars-unset-pairs' to unset env vars."
  (with-process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")
    (envars-unset-pairs '(("FOO" "foo") ("BAR" "barrr")))
    (should (equal nil (getenv "FOO")))
    (should (equal nil (getenv "BAR")))
    (should (equal "baz" (getenv "BAZ")))))

(ert-deftest envars--unset-names/simple ()
  "Test running `envars-unset-names' to unset env vars."
  (with-process-environment '("FOO=foo" "BAR=bar" "BAZ=baz")
    (envars--unset-names '("FOO" "BAR"))
    (should (equal nil (getenv "FOO")))
    (should (equal nil (getenv "BAR")))
    (should (equal "baz" (getenv "BAZ")))
    (should (equal '("BAZ=baz") process-environment))))

(ert-deftest envars--unset-names/non-existent-name ()
  "Test running `envars--unset-names' to unset non-existent env var.
This shouldn't cause a problem. The environment should remain
unchanged."
  (with-process-environment '("FOO=foo")
    (envars--unset-names '(("BAR" "bar")))
    (should (equal nil (getenv "BAR")))
    (should (equal '("FOO=foo") process-environment))))

(ert-deftest envars--export-pair ()
  "Test running `envars--export-pair' to set a single environment variable."
  (with-process-environment '()
    (envars--export-pair '("FOO" "foo"))
    (should (equal "foo" (getenv "FOO")))

    (envars--export-pair '("FOO" 1))
    (should (equal "1" (getenv "FOO")))))

(ert-deftest envars--unset-name/simple ()
  "Test running `envars--unset-name' to unset a single,
simple environment variable."
  (with-process-environment '("CATS=cats")
    (should (equal "cats" (getenv "CATS")))
    (envars--unset-name "CATS")
    (should (equal nil (getenv "CATS")))))

(ert-deftest envars--unset-name/multibyte ()
  "Test running `envars--unset-name' to unset a single,
multibyte environment variable."
  (with-process-environment '("\320\224=\320\244")
    (should (equal "Ф" (getenv "Д")))
    (envars--unset-name "Д")
    (should (equal nil (getenv "Д")))))

;;; envars-test.el ends here
