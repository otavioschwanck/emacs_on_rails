(use-package magit
  :defer 2
  :init
  (global-set-key (kbd "C-c g") 'magit-status)
  )

(use-package git-gutter
  :after (magit)
  :config
  (global-git-gutter-mode +1)
  )
