;;; apidoc-checker.el --- Flycheck checker for Apidoc specifications.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Package-Requires: ((flycheck "26-cvs") (js2-mode "20150909"))

;; Version: 0.2.0

;; Author: Chris Barrett <chris.d.barrett@me.com>

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

;; Flycheck checker for Apidoc specifications.

;;; Code:

(require 'dash)
(require 'flycheck)

(flycheck-define-checker apidoc
  "A syntax checker for Apidoc specifications. "
  :command ("apidoc-checker" "--plain" source)
  :predicate
  (lambda ()
    (let ((filename (file-name-nondirectory (buffer-file-name))))
      (member filename '("api.json" "apidoc.json"))))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
   (info line-start (file-name) ":" line ":" column ": note: " (message) line-end))
  :modes (js-mode js2-mode))

(add-to-list 'flycheck-checkers 'apidoc)

(provide 'apidoc-checker)

;;; apidoc-checker.el ends here
