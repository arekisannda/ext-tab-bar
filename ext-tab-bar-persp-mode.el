;;; ext-tab-bar-persp-mode.el --- ext-tab-bar persp-mode integration -*- lexical-binding: t -*-

;; Author: Alexander Chan
;; Maintainer: Alexander Chan

;;; Commentary:

;;; Code:

(require 'ext-tab-bar)
(require 'persp-mode)

(defface ext-tab-bar-faces-perspective
  '((((background light)) :foreground "#0fffff"
     :inherit ext-tab-bar-faces-default)
    (((background dark)) :foreground "#00bfff"
     :inherit ext-tab-bar-faces-default)
    (t :foreground "#00bfff"
       :inherit ext-tab-bar-faces-default))
  "Face for perspective segment."
  :group 'ext-tab-bar-faces)

(defun ext-tab-bar-perspective-segment ()
  "Perspective segment function."
  (propertize (format "[%s]" (safe-persp-name (get-current-persp)))
              'face '(nil :inherit (ext-tab-bar-faces-default
                                    ext-tab-bar-faces-perspective))))

;;;###autoload
(defun ext-tab-bar-persp-mode-setup ()
  "Set up `ext-tab-bar-mode` for `persp-mode`."
  (interactive)
  (setq ext-tab-bar-segment-list '(ext-tab-bar-debug-segment
                                   ext-tab-bar-perspective-segment
                                   ext-tab-bar-project-segment)))

(provide 'ext-tab-bar-persp-mode)

;;; ext-tab-bar-persp-mode.el ends here
