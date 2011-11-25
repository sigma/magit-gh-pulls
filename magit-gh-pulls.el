;;; magit-pulls.el --- git-pulls plug-in for Magit

;; Copyright (C) 2011  Yann Hodique

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

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
    (let* ((api (gh-pulls-api "api" :sync t))
           (repo (magit-gh-pulls-guess-repo))
           (reqs (oref (gh-pulls-list api (car repo) (cdr repo)) :data)))
      (dolist (req reqs)
        (magit-with-section (oref req :number) 'pull
          (magit-set-section-info req)
          (insert (format "\t[%s] %s\n"
                          (oref req :number)
                          (oref req :title)) )))))
  (insert "\n"))

;;;###autoload
(define-minor-mode magit-gh-pulls-mode "Pull requests support for Magit"
  :lighter " Pulls" :require 'magit-gh-pulls
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
