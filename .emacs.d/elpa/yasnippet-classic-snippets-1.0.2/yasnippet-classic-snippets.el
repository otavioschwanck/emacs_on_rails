;;; yasnippet-classic-snippets.el --- "Classic" yasnippet snippets

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Maintainer: Noam Postavsky <npostavs@gmail.com>
;; Keywords: snippets
;; Version: 1.0.2
;; Package-Requires: ((yasnippet "0.9.1"))
;; Package-Type: multi
;; Keywords: convenience, snippets

;;; Commentary:

;; Snippets that were previously shipped with the GNU ELPA yasnippet package.

;;; License:

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

;;; Code:

(defconst yasnippet-classic-snippets-dir
  (expand-file-name
   "snippets"
   (file-name-directory (or load-file-name buffer-file-name))))

(eval-after-load 'yasnippet
  '(progn
     (when (stringp yas-snippet-dirs)
       ;; In case the user set the old format.
       (setq yas-snippet-dirs (list yas-snippet-dirs)))
     (unless (memq 'yasnippet-classic-snippets-dir yas-snippet-dirs)
       ;; Prepare for future snippet reloads.
       (add-to-list 'yas-snippet-dirs 'yasnippet-classic-snippets-dir t)
       ;; And get our snippets ready now.
       (yas-load-directory yasnippet-classic-snippets-dir t))))

;;;###autoload (eval-after-load 'yasnippet '(require 'yasnippet-classic-snippets))

(provide 'yasnippet-classic-snippets)

;;; yasnippet-classic-snippets.el ends here
