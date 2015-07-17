(require 'ert)
(require 'magit-gh-pulls)

(ert-deftest magit-gh-pulls-collect-hostnames-test ()
  (let* ((sample-input '("extraneous" "Hostname one.other.com" "User doesntmatter" "HostName two.other.com"))
         (sample-data-end (-magit-gh-pulls-filter-and-split-host-lines sample-input))
         (more-input '("Host two" "Hostname should.not.show.up"))
         (sample-data (-magit-gh-pulls-filter-and-split-host-lines (append sample-input more-input))))
    (should (equal (magit-gh-pulls-collect-hostnames sample-data-end) '(() ("two.other.com" "one.other.com"))))
    (should (equal (magit-gh-pulls-collect-hostnames sample-data) (list (-magit-gh-pulls-filter-and-split-host-lines more-input) '("two.other.com" "one.other.com"))))))

(ert-deftest magit-gh-pulls-host-hostnames-test ()
  (let* ((sample-input '("Host one" "extraneous" "\tHostname one.other.com" "\t\tUser doesntmatter" "HostName two.other.com"))
         (sample-data (-magit-gh-pulls-filter-and-split-host-lines sample-input))
         (more-input '("Sir not appearing in this picture" "Host two" "IdentityFile ~/.ssh/HostName" "\tUser Host" "HostName two.host.com"))
         (bigger-sample (-magit-gh-pulls-filter-and-split-host-lines (append sample-input more-input))))
    (should (equal (magit-gh-pulls-get-host-hostnames sample-data) '(("one" . ("two.other.com" "one.other.com")))))
    (should (equal (magit-gh-pulls-get-host-hostnames bigger-sample) '(("two" . ("two.host.com")) ("one" . ("two.other.com" "one.other.com")))))))

(ert-deftest magit-gh-pulls-parse-url-test ()
  ;;Mock config hosts
  (let ((ssh-config-hosts '(("one" . ("github.com")))))
    (should (equal (magit-gh-pulls-parse-url "https://someotherhub.com/a/b" ssh-config-hosts) nil))
    (should (equal (magit-gh-pulls-parse-url "https://someotherhub.com/a/b.git" ssh-config-hosts) nil))
    (should (equal (magit-gh-pulls-parse-url "http://github.com/a/b.git" ssh-config-hosts) '("a" . "b")))
    (should (equal (magit-gh-pulls-parse-url "http://github.com/a/b" ssh-config-hosts) '("a" . "b")))
    (should (equal (magit-gh-pulls-parse-url "git://github.com/hi/there" ssh-config-hosts) '("hi" . "there")))
    (should (equal (magit-gh-pulls-parse-url "user@github.com:ssh/repo" ssh-config-hosts) '("ssh" . "repo")))
    (should (equal (magit-gh-pulls-parse-url "user@one:ssh/alias.git" ssh-config-hosts) '("ssh" . "alias")))))


(ert-deftest test-magit-gh-pulls-parse-url-git-at ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "git@github.com:sigma/magit-gh-pulls.git" '()))))

(ert-deftest test-magit-gh-pulls-parse-url-https ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "https://github.com/sigma/magit-gh-pulls.git" '()))))

(ert-deftest test-magit-gh-pulls-parse-url-https ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "https://github.com/sigma/magit-gh-pulls/" '()))))

(ert-deftest test-magit-gh-pulls-parse-url-http ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "http://github.com/sigma/magit-gh-pulls.git" '()))))

(ert-deftest test-magit-gh-pulls-parse-url-git ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "git://github.com/sigma/magit-gh-pulls.git" '()))))

(ert-deftest test-magic-gh-pulls-parse-url-invalid ()
  (should (eq nil (magit-gh-pulls-parse-url "http://google.com" '()))))

(ert-deftest test-magic-gh-pulls-parse-url-garbage ()
  (should (eq nil (magit-gh-pulls-parse-url "08h3fiuandiu" '()))))
