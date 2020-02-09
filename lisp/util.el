(defun iwb ()
  "INDENT WHOLE BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  )

(global-set-key (kbd "C-=") 'iwb)
(global-set-key (kbd "C-+") 'delete-trailing-whitespace)
(global-set-key (kbd "C-S-k") 'sp-kill-hybrid-sexp)

(global-hl-line-mode +1)

;;; Reload Buffer
(global-set-key (kbd "C-x r <RET>") #'msc/revert-buffer-noconfirm)

(defun msc/revert-buffer-noconfirm ()
  "Call `revert-buffer' with the NOCONFIRM argument set."
  (interactive)
  (revert-buffer nil t))

;;; hide dired detail
(add-hook 'dired-mode-hook
          (lambda () (dired-hide-details-mode +1)))


;;; Drag Stuff
(use-package drag-stuff
  :init
  (drag-stuff-global-mode)
  (global-set-key (kbd "M-P") 'drag-stuff-up)
  (global-set-key (kbd "M-N") 'drag-stuff-down)
  (global-set-key [M-S-up] 'drag-stuff-up)
  (global-set-key [M-S-down] 'drag-stuff-down)
  )
