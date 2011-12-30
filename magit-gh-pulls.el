;;; magit-pulls.el --- git-pulls plug-in for Magit

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:
;; Version: 0.2
;; Package-Requires: ((gh "0.4.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'eieio)

(require 'magit)
(require 'gh-pulls)

(defun magit-gh-pulls-guess-repo ()
  (let* ((cfg (magit-get "magit" "gh-pulls-repo"))
         (split (split-string cfg "/")))
    (cons (car split) (cadr split))))

(magit-define-inserter gh-pulls ()
  (magit-with-section "Pull Requests" 'pulls
    (insert (propertize "Pull Requests:\n" 'face 'magit-section-title))
    (let* ((api (gh-pulls-api "api" :sync t :cache t))
           (repo (magit-gh-pulls-guess-repo))
           (user (car repo))
           (proj (cdr repo))
           (stubs (oref (gh-pulls-list api user proj) :data)))
      (dolist (stub stubs)
        (let* ((id (oref stub :number))
               (req (oref (gh-pulls-get api user proj id) :data))
               (base-sha (oref (oref req :base) :sha))
               (head-sha (oref (oref req :head) :sha))
               ;; branch has been deleted in the meantime...
               (invalid (equal (oref (oref req :head) :ref) head-sha))
               (have-commits
                (and (eql 0 (magit-git-exit-code "cat-file" "-e" base-sha))
                     (eql 0 (magit-git-exit-code "cat-file" "-e" head-sha))))
               (header (propertize (format "\t[%s] %s\n" id (oref req :title))
                                   'face (cond (have-commits 'default)
                                               (invalid 'error)
                                               (t 'italic)))))
          (magit-with-section id (cond (have-commits 'pull)
                                       (invalid 'invalid-pull)
                                       (t 'unfetched-pull))
            (magit-set-section-info (list user proj id))
            (insert header)
            (when have-commits
              (apply #'magit-git-section
                     'request nil 'magit-wash-log "log"
                     (append magit-git-log-options
                             (list
                              "--reverse"
                              (format "%s..%s" base-sha head-sha))))))))))
  (insert "\n"))

(defun magit-gh-pulls-guess-topic-name (req)
  (let ((user (oref (oref req :user) :login))
        (topic (oref (oref req :head) :ref)))
    (format "%s/%s" user topic)))

(defun magit-gh-pulls-create-branch ()
  (interactive)
  (magit-section-action (item info "ghpr")
    ((pull)
     (let* ((api (gh-pulls-api "api" :sync t :cache t))
            (req (oref (apply 'gh-pulls-get api info) :data))
            (branch (read-from-minibuffer
                     "Branch name: " (magit-gh-pulls-guess-topic-name req)))
            (base (magit-read-rev "Branch base: "
                                  (oref (oref req :base) :ref))))
       (magit-create-branch branch base)
       (magit-merge (oref (oref req :head) :sha))))
    ((unfetched-pull)
     (error "Please fetch pull request commits first"))
    ((invalid-pull)
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-fetch-commits ()
  (interactive)
  (magit-section-action (item info "ghpr")
    ((unfetched-pull)
     (let* ((api (gh-pulls-api "api" :sync t :cache t))
            (req (oref (apply 'gh-pulls-get api info) :data))
            (head (oref req :head)))
       (magit-run-git "fetch" (oref (oref head :repo) :git-url)
                      (oref head :ref))))
    ((pull)
     nil)
    ((invalid-pull)
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-purge-cache ()
  (let* ((api (gh-pulls-api "api" :sync t :cache t))
         (cache (oref api :cache))
         (repo (magit-gh-pulls-guess-repo)))
    (pcache-map cache (lambda (k v)
                        (when (string-match
                               (format "/repos/%s/%s/" (car repo) (cdr repo))
                               (car k))
                          (pcache-invalidate cache k))))))

(defun magit-gh-pulls-reload ()
  (interactive)
  (magit-with-refresh
    (magit-gh-pulls-purge-cache)
    (magit-need-refresh)))

(easy-menu-define magit-gh-pulls-extension-menu
  nil
  "GitHub Pull Requests extension menu"
  '("GitHub Pull Requests"
    :visible magit-gh-pulls-mode
    ["Reload pull request" magit-gh-pulls-reload]
    ["Create pull request branch" magit-gh-pulls-create-branch]
    ["Fetch pull request commits" magit-gh-pulls-fetch-commits]
    ))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-gh-pulls-extension-menu)

(defvar magit-gh-pulls-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "# g b") 'magit-gh-pulls-create-branch)
    (define-key map (kbd "# g f") 'magit-gh-pulls-fetch-commits)
    (define-key map (kbd "# g g") 'magit-gh-pulls-reload)
    map))

;;;###autoload
(define-minor-mode magit-gh-pulls-mode "Pull requests support for Magit"
  :lighter " Pulls" :require 'magit-gh-pulls :keymap 'magit-gh-pulls-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-gh-pulls-mode
      (progn
        (add-hook 'magit-before-insert-stashes-hook 'magit-insert-gh-pulls nil t))
    (progn
      (remove-hook 'magit-before-insert-stashes-hook 'magit-insert-gh-pulls t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-gh-pulls ()
  "Unconditionally turn on `magit-pulls-mode'."
  (magit-gh-pulls-mode 1))

(provide 'magit-gh-pulls)
;;; magit-pulls.el ends here
