;;; ext-tab-bar-persp-mode.el --- ext-tab-bar persp-mode integration -*- lexical-binding: t -*-

;; Author: Alexander Chan
;; Maintainer: Alexander Chan

;;; Commentary:

;;; Code:

(require 'ext-tab-bar)
(require 'tab-bar)

(defface ext-tab-bar-faces-perspective
  '((((background light)) :foreground "#0fffff"
     :inherit ext-tab-bar-faces-default)
    (((background dark)) :foreground "#00bfff"
     :inherit ext-tab-bar-faces-default)
    (t :foreground "#00bfff"
       :inherit ext-tab-bar-faces-default))
  "Face for perspective segment."
  :group 'ext-tab-bar-faces)

;;; --- persp-mode.el ---

(defun ext-tab-bar--persp-mode-segment ()
  "`persp-mode` segment function."
  (propertize (format "[%s]" (safe-persp-name (get-current-persp)))
              'face '(nil :inherit (ext-tab-bar-faces-default
                                    ext-tab-bar-faces-perspective))))

(defun ext-tab-bar--persp-mode-before-deactivate (_)
  "Save `tab-bar` state to `persp-mode` state object before perspective deactivates."
  (when (get-current-persp)
    (set-persp-parameter 'tab-bar-tabs (tab-bar-tabs))))

(defun ext-tab-bar--persp-mode-activated (_)
  "Load `tab-bar` state from `persp-mode` state object when perspective activates."
  (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
  (tab-bar--update-tab-bar-lines t))

(defun ext-tab-bar--persp-mode-before-save-to-file  (&rest _)
  "Save filtered `tab-bar` state to `persp-mode` state object before save to file."
  (when (get-current-persp)
    (set-persp-parameter
     'tab-bar-tabs
     (frameset-filter-tabs (tab-bar-tabs) nil nil t))))

(defun ext-tab-bar--persp-mode-teardown ()
  "Teardown function to remove `ext-tab-bar` setup for `persp-mode`."
  (remove-hook 'persp-before-deactivate-functions
               #'ext-tab-bar--persp-mode-before-deactivate)
  (remove-hook 'persp-activated-functions
               #'ext-tab-bar--persp-mode-activated)
  (remove-hook 'persp-before-save-state-to-file-functions
               #'ext-tab-bar--persp-mode-before-save-to-file)
  (remove-hook 'ext-tab-bar-teardown-hook
               #'ext-tab-bar--persp-mode-teardown)
  (setq ext-tab-bar-segment-list (cl-remove #'ext-tab-bar--persp-mode-segment
                                            ext-tab-bar-segment-list)))

(defun ext-tab-bar--persp-mode-setup ()
  "Activate`persp-mode` extension."
  (add-hook 'persp-before-deactivate-functions
            #'ext-tab-bar--persp-mode-before-deactivate)
  (add-hook 'persp-activated-functions
            #'ext-tab-bar--persp-mode-activated)
  (add-hook 'persp-before-save-state-to-file-functions
            #'ext-tab-bar--persp-mode-before-save-to-file)
  (add-hook 'ext-tab-bar-teardown-hook
            #'ext-tab-bar--persp-mode-teardown)
  (setq ext-tab-bar-segment-list '(ext-tab-bar-debug-segment
                                   ext-tab-bar--persp-mode-segment
                                   ext-tab-bar-project-segment)))

;;; --- end persp-mode.el ---

;;;###autoload
(defun ext-tab-bar-persp-mode-setup ()
  "Enable `ext-tab-bar` perspective extension."
  (interactive)
  (cond ((featurep 'persp-mode) (ext-tab-bar--persp-mode-setup))
        (t (user-error "Unable to enable extension: missing `persp-mode` or `perspective`"))))

;;;###autoload
(defun ext-tab-bar-persp-mode-teardown ()
  "Disable `ext-tab-bar` perspective extension."
  (interactive)
  (cond ((featurep 'persp-mode) (ext-tab-bar--persp-mode-teardown))
        (t (user-error "Unable to disable extension: missing `persp-mode` or `perspective`"))))

(provide 'ext-tab-bar-persp-mode)

;;; ext-tab-bar-persp-mode.el ends here
