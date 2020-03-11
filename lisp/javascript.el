(use-package js2-mode
  :defer 3
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
  )

(use-package js2-refactor
  :defer 3
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c j")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  )

(use-package xref-js2
  :config
  (add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
  )

(use-package tide
  :defer 2
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
