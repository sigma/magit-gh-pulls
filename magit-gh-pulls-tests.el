(require 'ert)
(require 'magit-gh-pulls)

(ert-deftest test-magit-gh-pulls-parse-url-git-at ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "git@github.com:sigma/magit-gh-pulls.git"))))

(ert-deftest test-magit-gh-pulls-parse-url-https ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "https://github.com/sigma/magit-gh-pulls.git"))))

(ert-deftest test-magit-gh-pulls-parse-url-https ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "https://github.com/sigma/magit-gh-pulls/"))))

(ert-deftest test-magit-gh-pulls-parse-url-http ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "http://github.com/sigma/magit-gh-pulls.git"))))

(ert-deftest test-magit-gh-pulls-parse-url-git ()
  (should (equal '("sigma" . "magit-gh-pulls")
                 (magit-gh-pulls-parse-url "git://github.com/sigma/magit-gh-pulls.git"))))

(ert-deftest test-magic-gh-pulls-parse-url-invalid ()
  (should (eq nil (magit-gh-pulls-parse-url "http://google.com"))))

(ert-deftest test-magic-gh-pulls-parse-url-garbage ()
  (should (eq nil (magit-gh-pulls-parse-url "08h3fiuandiu"))))

