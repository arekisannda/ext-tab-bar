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

(defvar ext-tab-bar-persp-mode-hash (make-hash-table :test 'equal))
(defvar ext-tab-bar-persp-mode-killed-hash (make-hash-table :test 'equal))
(defvar ext-tab-bar-persp-mode--previous-name nil)

(defun ext-tab-bar-persp-mode-curr-name ()
  "Return name of current perspective."
  (cond ((featurep 'persp-mode) (safe-persp-name (get-current-persp)))
        ((featurep 'perspective) (persp-name (persp-curr)))))

(defun ext-tab-bar-persp-mode-list-persp-names ()
  "Return list of perspective names."
  (cond ((featurep 'persp-mode) (persp-names))
        ((featurep 'perspective) (persp-names))))

(defun ext-tab-bar-persp-mode-segment ()
  "Perspective segment function."
  (propertize (format "[%s]" (ext-tab-bar-persp-mode-curr-name))
              'face '(nil :inherit (ext-tab-bar-faces-default
                                    ext-tab-bar-faces-perspective))))

(defun ext-tab-bar-persp-mode-load ()
  "Activate current perspective's `tab-bar` configurations."
  (let* ((persp-name (ext-tab-bar-persp-mode-curr-name))
         (conf (gethash persp-name ext-tab-bar-persp-mode-hash)))
    (tab-bar-tabs-set conf)
    (tab-bar--update-tab-bar-lines t)))

(defun ext-tab-bar-persp-mode-update ()
  "Save perspective's `tab-bar` configuration before switching."
  (let ((persp (ext-tab-bar-persp-mode-curr-name)))
    (if (gethash persp ext-tab-bar-persp-mode-killed-hash)
        (if (gethash persp ext-tab-bar-persp-mode-hash)
            (remhash persp ext-tab-bar-persp-mode-hash))
      (puthash persp (frameset-filter-tabs (tab-bar-tabs) nil nil t)
               ext-tab-bar-persp-mode-hash))))

(defun ext-tab-bar-persp-mode-add (&optional persp)
  "Save new PERSP's `tab-bar` configuration."
  (let* ((persp (or persp (ext-tab-bar-persp-mode-curr-name)))
         (killed (gethash persp ext-tab-bar-persp-mode-killed-hash))
         (conf (gethash persp ext-tab-bar-persp-mode-hash)))
    (cond ((and persp killed)
           ;; additiion of perspective with the name of a previously removed perspective
           ;; remove perspective from killed-hash and use nil `tab-bar` config
           (remhash persp ext-tab-bar-persp-mode-killed-hash)
           (puthash persp nil ext-tab-bar-persp-mode-hash))
          ((and persp conf)
           (puthash persp conf ext-tab-bar-persp-mode-hash)))))

(defun ext-tab-bar-persp-mode-load-from-state-hash ()
  "Load `ext-tab-bar` perspective state table."
  (setq ext-tab-bar-persp-mode-hash (gethash "perspectives" ext-tab-bar-state-hash))
  (ext-tab-bar-persp-mode-load))

(defun ext-tab-bar-persp-mode-save-to-state-hash ()
  "Update pespectives' `tab-bar` state in `ext-tab-bar-state-hash`."
  (ext-tab-bar-persp-mode-update)

  (let ((tmp-hash (make-hash-table :test 'equal)))
    (dolist (persp-name (ext-tab-bar-persp-mode-list-persp-names))
      (puthash persp-name (gethash persp-name ext-tab-bar-persp-mode-hash) tmp-hash))
    (setq ext-tab-bar-persp-mode-hash tmp-hash))

  (puthash "perspectives" ext-tab-bar-persp-mode-hash ext-tab-bar-state-hash))

(defun ext-tab-bar-persp-mode-rename-set-previous-name ()
  "Save a perspective's old name before it is renamed."
  (setq ext-tab-bar-persp-mode--previous-name (ext-tab-bar-persp-mode-curr-name)))

(defun ext-tab-bar-persp-mode-rename (&optional old-name new-name)
  "Update perspective's OLD-NAME to NEW-NAME in `ext-tab-bar-persp-mode-hash`."
  (let ((old-conf (gethash old-name ext-tab-bar-persp-mode-hash))
        (old-name (or old-name ext-tab-bar-persp-mode--previous-name))
        (new-name (or new-name (ext-tab-bar-persp-mode-curr-name))))
    (when old-conf
      (puthash new-name old-conf ext-tab-bar-persp-mode-hash))
    (remhash old-name ext-tab-bar-persp-mode-hash))
  (setq ext-tab-bar-persp-mode--previous-name nil))

(defun ext-tab-bar-persp-mode-kill (&optional persp)
  "Remove PERSP's configuration from `ext-tab-bar-persp-mode-hash`."
  (let* ((persp (or persp (ext-tab-bar-persp-mode-curr-name))))
    (puthash persp t ext-tab-bar-persp-mode-killed-hash)
    (remhash persp ext-tab-bar-persp-mode-hash)))

;;; --- persp-mode.el ---

(defun ext-tab-bar--save-state-to-file-persp-mode (&optional _fname _hash _set)
  "`persp-mode` variant of `ext-tab-bar-save-state-to-file`."
  (ext-tab-bar-save-state-to-file))

(defun ext-tab-bar-persp-mode-load--persp-mode (&optional _frame-or-window)
  "`persp-mode` variant of `ext-tab-bar-persp-mode-load`."
  (ext-tab-bar-persp-mode-load))

(defun ext-tab-bar-persp-mode-update--persp-mode (&optional _frame-or-window)
  "`persp-mode` variant of `ext-tab-bar-persp-mode-update`."
  (ext-tab-bar-persp-mode-update))

(defun ext-tab-bar-persp-mode-add--persp-mode (&optional _persp _hash)
  "`persp-mode` variant of `ext-tab-bar-persp-mode-add`."
  (ext-tab-bar-persp-mode-add (safe-persp-name _persp)))

(defun ext-tab-bar-persp-mode-rename--persp-mode (&optional _persp _old-name _new-name)
  "`persp-mode` variant of `ext-tab-bar-persp-mode-rename`."
  (ext-tab-bar-persp-mode-rename _old-name _new-name))

(defun ext-tab-bar-persp-mode-kill--persp-mode (&optional _persp)
  "`persp-mode` variant of `ext-tab-bar-persp-mode-kill`."
  (ext-tab-bar-persp-mode-kill (and _persp (safe-persp-name _persp))))

(defun ext-tab-bar-persp-mode-teardown--persp-mode ()
  "Teardown function to remove `ext-tab-bar` setup for `persp-mode`."
  (remove-hook 'persp-created-functions #'ext-tab-bar-persp-mode-add--persp-mode)
  (remove-hook 'persp-before-deactivate-functions #'ext-tab-bar-persp-mode-update--persp-mode)
  (remove-hook 'persp-activated-functions #'ext-tab-bar-persp-mode-load--persp-mode)
  (remove-hook 'persp-before-save-state-to-file-functions #'ext-tab-bar--save-state-to-file-persp-mode)
  (remove-hook 'persp-renamed-functions #'ext-tab-bar-persp-mode-rename--persp-mode)
  (remove-hook 'persp-before-kill-functions #'ext-tab-bar-persp-mode-kill--persp-mode))

(defun ext-tab-bar-persp-mode-setup--persp-mode ()
  "Activate `persp-mode` extension."
  (add-hook 'persp-created-functions #'ext-tab-bar-persp-mode-add--persp-mode)
  (add-hook 'persp-before-deactivate-functions #'ext-tab-bar-persp-mode-update--persp-mode)
  (add-hook 'persp-activated-functions #'ext-tab-bar-persp-mode-load--persp-mode)
  (add-hook 'persp-before-save-state-to-file-functions #'ext-tab-bar--save-state-to-file-persp-mode)
  (add-hook 'persp-renamed-functions #'ext-tab-bar-persp-mode-rename--persp-mode)
  (add-hook 'persp-before-kill-functions #'ext-tab-bar-persp-mode-kill--persp-mode))

;;; --- end persp-mode.el ---

;;; --- perspective.el ---

(defun ext-tab-bar-persp-mode-teardown--perspective ()
  "Teardown function to remove `ext-tab-bar` setup for `perspective`."
  (remove-hook 'persp-created-hook #'ext-tab-bar-persp-mode-add)
  (remove-hook 'persp-before-switch-hook #'ext-tab-bar-persp-mode-update)
  (remove-hook 'persp-activated-hook #'ext-tab-bar-persp-mode-load)
  (remove-hook 'persp-state-before-save-hook #'ext-tab-bar-save-state-to-file)
  (remove-hook 'persp-before-rename-hook #'ext-tab-bar-persp-mode-rename-set-previous-name)
  (remove-hook 'persp-after-rename-hook #'ext-tab-bar-persp-mode-rename)
  (remove-hook 'persp-killed-hook #'ext-tab-bar-persp-mode-kill))

(defun ext-tab-bar-persp-mode-setup--perspective ()
  "Activate `perspective` extension."
  (add-hook 'persp-created-hook #'ext-tab-bar-persp-mode-add)
  (add-hook 'persp-before-switch-hook #'ext-tab-bar-persp-mode-update)
  (add-hook 'persp-activated-hook #'ext-tab-bar-persp-mode-load)
  (add-hook 'persp-state-before-save-hook #'ext-tab-bar-save-state-to-file)
  (add-hook 'persp-before-rename-hook #'ext-tab-bar-persp-mode-rename-set-previous-name)
  (add-hook 'persp-after-rename-hook #'ext-tab-bar-persp-mode-rename)
  (add-hook 'persp-killed-hook #'ext-tab-bar-persp-mode-kill))

;;; --- end perspective.el ---

;;;###autoload
(defun ext-tab-bar-persp-mode-setup ()
  "Enable `ext-tab-bar` perspective extension."
  (interactive)
  (add-hook 'ext-tab-bar-after-load-hook #'ext-tab-bar-persp-mode-load-from-state-hash)
  (add-hook 'ext-tab-bar-before-save-hook #'ext-tab-bar-persp-mode-save-to-state-hash)
  (add-hook 'ext-tab-bar-teardown-hook #'ext-tab-bar-persp-mode-teardown)
  (setq ext-tab-bar-segment-list '(ext-tab-bar-debug-segment
                                   ext-tab-bar-persp-mode-segment
                                   ext-tab-bar-project-segment))

  (cond ((featurep 'persp-mode) (ext-tab-bar-persp-mode-setup--persp-mode))
        ((featurep 'perspective) (ext-tab-bar-persp-mode-setup--perspective))
        (t (user-error "Unable to enable extension: missing `persp-mode` or `perspective`"))))

;;;###autoload
(defun ext-tab-bar-persp-mode-teardown ()
  "Disable `ext-tab-bar` perspective extension."
  (interactive)
  (remove-hook 'ext-tab-bar-after-load-hook #'ext-tab-bar-persp-mode-load-from-state-hash)
  (remove-hook 'ext-tab-bar-before-save-hook #'ext-tab-bar-persp-mode-save-to-state-hash)
  (remove-hook 'ext-tab-bar-teardown-hook #'ext-tab-bar-persp-mode-teardown)
  (setq ext-tab-bar-segment-list (cl-remove #'ext-tab-bar-persp-mode-segment
                                            ext-tab-bar-segment-list))

  (cond ((featurep 'persp-mode) (ext-tab-bar-persp-mode-teardown--persp-mode))
        ((featurep 'perspective) (ext-tab-bar-persp-mode-teardown--perspective))
        (t (user-error "Unable to disable extension: missing `persp-mode` or `perspective`"))))

(provide 'ext-tab-bar-persp-mode)

;;; ext-tab-bar-persp-mode.el ends here
