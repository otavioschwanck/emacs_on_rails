(add-hook 'scss-mode-hook
          '(lambda()
             (setq tab-width 2)
             (setq indent-tabs-mode nil)))

(use-package zencoding-mode
  :config
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (define-key web-mode-map (kbd "C-c C-v") #'zencoding-expand-line)
  )
