;;; ext-tab-bar.el --- Extend tab-bar -*- lexical-binding: t -*-

;; Author: Alexander Chan
;; Maintainer: Alexander Chan

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Provides `ext-tab-bar-mode` and tools to extend the global tab bar with
;; extra information.

;;; Code:

(unless (version<= "28.1" emacs-version)
  (error "This package requires Emacs 28.1 or later"))

(defgroup ext-tab-bar nil
  "Frame-local tabs with workspace and project information."
  :group 'convenience
  :prefix "ext-tab-bar-")

(defgroup ext-tab-bar-faces nil
  "Faces used in project tab bar."
  :group 'ext-tab-bar
  :group 'faces
  :prefix "ext-tab-bar-faces-")

(defface ext-tab-bar-faces-default
  `((t :weight normal
       :height 1.0))
  "Face for segment."
  :group 'ext-tab-bar-faces)

(defface ext-tab-bar-faces-debug
  '((((background light)) :foreground "#ff0000"
     :inherit ext-tab-bar-faces-default)
    (((background dark)) :foreground "#b22222"
     :inherit ext-tab-bar-faces-default)
    (t :foreground "#b22222"
       :inherit ext-tab-bar-faces-default))
  "Face for debug segment."
  :group 'ext-tab-bar-faces)

(defface ext-tab-bar-faces-project
  '((((background light)) :foreground "#00ff00"
     :inherit ext-tab-bar-faces-default)
    (((background dark)) :foreground "#8fbc8f"
     :inherit ext-tab-bar-faces-default)
    (t :foreground "#8fbc8f"
       :inherit ext-tab-bar-faces-default))
  "Face for project segment."
  :group 'ext-tab-bar-faces)

(defcustom ext-tab-bar-location 'tab-bar-format-align-right
  "Location to display `ext-tab-bar` information."
  :group 'ext-tab-bar
  :type
  '(choice
    (const :tag "Insert after tab-bar-format-add-tab" tab-bar-format-add-tab)
    (const :tag "Insert after tab-bar-format-align-right" tab-bar-format-align-right)
    (const :tag "Insert after tab-bar-format-global" tab-bar-format-global)
    (function :tag "Insert after function")
    (const :tag "Insert as first element" beginning)
    (const :tag "Insert as last element" end)
    (const :tag "Replace all other elements" replace)))

(defcustom ext-tab-bar-segment-list '(ext-tab-bar-debug-segment
                                      ext-tab-bar-project-segment)
  "`ext-tab-bar` segment list."
  :type '(repeat function)
  :group 'ext-tab-bar)

(defun ext-tab-bar-debug-segment ()
  "Debug segment function."
  (propertize (if init-file-debug "[DEBUG]" "")
              'face '(nil :inherit (ext-tab-bar-faces-default
                                    ext-tab-bar-faces-debug))))

(defcustom ext-tab-bar-project-default "--"
  "String for when project is not set."
  :type 'string
  :group 'ext-tab-bar)

(defun ext-tab-bar--format-project-string (project-dir)
  "Format PROJECT-DIR string."
  (let* ((suffix "/"))
    (if (not (or (not project-dir) (string-blank-p project-dir)))
        (if (string-suffix-p suffix project-dir)
            (substring project-dir 0 (- (length project-dir) (length suffix)))
          project-dir))))

(defun ext-tab-bar-project-segment ()
  "Project segment function."
  (let* ((current-project (project-current))
         (project-name ext-tab-bar-project-default))
    (when current-project
      (setq project-name (project-root current-project)))
    (propertize (format "[%s]" (ext-tab-bar--format-project-string project-name))
                'face '(nil :inherit (ext-tab-bar-faces-default
                                      ext-tab-bar-faces-project)))))

(defun ext-tab-bar--active-frame-p ()
  "Return t if current frame is active frame."
  (cond ((boundp 'moody--active-window)
         (eq (window-frame) (window-frame moody--active-window)))
        ((boundp 'powerline-selected-window)
         (eq (window-frame) (window-frame powerline-selected-window)))
        (t t)))

(defun ext-tab-bar--format ()
  "Return extended tab-bar string."
  (mapconcat #'funcall ext-tab-bar-segment-list ""))

(defvar ext-tab-bar--temporary-bar nil)
(defvar ext-tab-bar--previous-format nil)

;;;###autoload
(define-minor-mode ext-tab-bar-mode
  "Show additional information in the tab bar."
  :global t
  (cond
   (ext-tab-bar-mode
    (unless tab-bar-mode
      (setq ext-tab-bar--temporary-bar t)
      (tab-bar-mode 1))
    (cl-case ext-tab-bar-location
      (replace
       (setq ext-tab-bar--previous-format tab-bar-format)
       (setq tab-bar-format (list #'ext-tab-bar)))
      (beginning
       (setq tab-bar-format (cons #'ext-tab-bar tab-bar-format)))
      (end
       (setq tab-bar-format (nconc tab-bar-format (list #'ext-tab-bar))))
      (t
       (let ((mem (memq ext-tab-bar-location tab-bar-format)))
         (if mem
             (setcdr mem (cl-pushnew #'ext-tab-bar (cdr mem)))
           (setq tab-bar-format
                 (nconc tab-bar-format
                        (if (eq ext-tab-bar-location
                                'tab-bar-format-align-right)
                            (list 'tab-bar-format-align-right
                                  'ext-tab-bar)
                          (message "%s not found in %s; adding at end instead"
                                   ext-tab-bar-location 'tab-bar-format)
                          (list #'ext-tab-bar)))))))))
   (t
    (when ext-tab-bar--temporary-bar
      (setq ext-tab-bar--temporary-bar nil)
      (tab-bar-mode -1))
    (cond (ext-tab-bar--previous-format
           (setq tab-bar-format ext-tab-bar--previous-format)
           (setq ext-tab-bar--previous-format nil))
          (t
           (setq tab-bar-format (delq #'ext-tab-bar tab-bar-format)))))))

(defun ext-tab-bar ()
  "Generate extended tab-bar."
  (and ext-tab-bar-mode
       (ext-tab-bar--active-frame-p)
       (ext-tab-bar--format)))

(provide 'ext-tab-bar)

;;; ext-tab-bar.el ends here
