;; Syntax Checkers

(setq-default flycheck-disabled-checkers '(ruby-reek)) ;; Comment here if  you use reek
;; (setq-default flycheck-disabled-checkers '(ruby-rubocop)) ;; uncomment here if dont use rubocop


;; By default, the emacs on rails use RBENV
;; We strongly recommend you to use rbenv instead rvm
(use-package rbenv
  :init
  (global-rbenv-mode)
  )

;; if you want to use rvm, just uncomment the lines below
;; (use-package rvm
  ;; :init
  ;; (rvm-use-default)
;; )


;; Change here the desired theme
(load-theme 'doom-one t)

;; This show the command pallet, it help for new comers.
(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.33
    which-key-idle-delay 0.05)
  (push '((nil . "projectile-rails-\\(.+\\)") . (nil . "\\1"))
        which-key-replacement-alist)
  :diminish which-key-mode)

;; Default projectile rails command.  If you use docker, can change here.
(defvar projectile-rails-vanilla-command "bin/rails")
