(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind (("C-c p" . projectile-command-map)
	 ("C-," . counsel-projectile-switch-to-buffer)
	 ("C-." . projectile-find-file)))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode)
  (setq projectile-globally-ignored-directories
      (append '(".bundle" ".vendor" "public" "node-modules")
              projectile-globally-ignored-directories))
  )

(use-package projectile-rails
  :init
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)
  (setq inf-ruby-console-environment "development")
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (global-set-key (kbd "C-c r 3 T") (kbd "C-c , 4 t"))
  )
