(use-package magit
  :init
  (global-set-key (kbd "C-c g") 'magit-status)
  )

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  )
