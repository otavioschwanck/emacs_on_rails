;;; uncomment this to disable startup and menu-bar

(menu-bar-mode -1)
(toggle-scroll-bar -1)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(put 'dired-find-alternate-file 'disabled nil)

;;; Helm

(require 'helm-config)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x b") #'helm-buffers-list)
(global-set-key (kbd "C-c r '") #'robe-start)

;;; Flycheck

(global-flycheck-mode)
(global-set-key "\C-\M-j" #'flycheck-next-error)
(global-set-key "\C-\M-i" #'flycheck-previous-error)

(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(eval-after-load 'flycheck
  (if (display-graphic-p)
      (flycheck-pos-tip-mode)
    (flycheck-popup-tip-mode)))

;; Comment here if  you use reek
(setq-default flycheck-disabled-checkers '(ruby-reek))

;;; Projectile

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defvar projectile-rails-vanilla-command "bin/rails")

(require 'helm-projectile)
(helm-projectile-on)

(projectile-rails-global-mode)

(setq projectile-globally-ignored-directories
      (append '(".bundle" ".vendor" "public" "node-modules")
              projectile-globally-ignored-directories))

(setq projectile-create-missing-test-files t)

(defun projectile-git-fetch ()
  (magit-status)
  (if (fboundp 'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-from-upstream)
    (call-interactively #'magit-fetch-current))
  )

(setq projectile-switch-project-action
      #'projectile-git-fetch)

(def-projectile-commander-method ?F
  "Git fetch."
  (projectile-git-fetch)
  )

;;; Run projectile fetch + robe-start
(setq inf-ruby-console-environment "development")

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

(def-projectile-commander-method ?S
  "Git fetch and console."
  (magit-status)
  (if (fboundp 'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-from-upstream)
    (call-interactively #'magit-fetch-current))
  (message "%s" (shell-command-to-string "docker-compose up -d"))
  (call-interactively 'projectile-rails-console)
  (other-window 1))


(require 'projectile)

;;; Company-mode

(require 'company)
(require 'company-tern)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

;; With this code, yasnippet will expand the snippet if company didn't complete the word
;; replace company-complete-common with company-complete if you're using it

(advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common :after (lambda ()
                                              (when (equal my-company-point (point))
                                                (yas-expand))))

;;; Robe
(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-to-list 'company-backends 'company-tern)

(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(global-robe-mode)

(setq compilation-scroll-output t)

;;; Snippet

(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)

(add-hook 'prog-mode-hook #'yas-minor-mode)

;;; Ruby

(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-add-encoding-comment-on-save nil)

(require 'rspec-mode)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(setq ruby-use-smie t)
(setq ruby-align-chained-calls t)
(setq rspec-use-bundler-when-possible t)
(setq rspec-use-spring-when-possible nil)

;;; Stop Locks and Backups

(setq auto-save-default nil) ; stop creating #autosave# files
(setq make-backup-files nil) ; stop creating backup~ files

(setq create-lockfiles nil)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;;; Gui Options

(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;;; Personal functions

(defun endless/toggle-ruby-block ()
  "Toggle between the two types of ruby blocks."
  (interactive)
  (while (not (looking-at-p "\\_<do\\_>\\|{"))
    (backward-up-list))
  (let ((l (point))
        (do? (eq (char-after) ?d))
        (r (progn (forward-sexp 1)
                  (point-marker))))
    (if do?
        (progn
          (delete-char -3)
          (hungry-delete-backward 1)
          (insert-before-markers " }")
          (goto-char l)
          (delete-char 2)
          (insert "{")
          (when (looking-at-p " +|")
            (forward-sexp 1))
          (insert " ")
          (hungry-delete-forward 1))
      (delete-char -1)
      (hungry-delete-backward 1)
      (insert-before-markers "\nend")
      (goto-char l)
      (delete-char 1)
      (insert "do")
      (when (looking-at-p " +|")
        (forward-sexp 1))
      (hungry-delete-forward 1)
      (insert "\n"))
    (indent-region l r)))



(defun iwb ()
  "INDENT WHOLE BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  )

(defun rubo_current ()
  "RUBOCOP ON CURRENT_FILE."
  (interactive)
  (save-buffer)
  (message "%s" (shell-command-to-string
                 (concat "bundle exec rubocop -a "
                         (shell-quote-argument (buffer-file-name)))))
  (msc/revert-buffer-noconfirm)
  )


(defun endless/ruby-define-method ()
  (interactive)
  (forward-sexp 1)
  (forward-sexp -1)
  (let ((l (point)))
    (forward-sexp 1)
    (when (looking-at-p "(")
      (forward-sexp 1))
    (let ((str (buffer-substring l (point))))
      (end-of-defun)
      (let ((l (point)))
        (insert "\ndef " str "\n")
        (save-excursion
          (insert "\nend\n")
          (indent-region l (point)))
        (indent-for-tab-command)))))

(defun endless/-ruby-symbol-at-point ()
  (let ((l (point)))
    (save-excursion
      (forward-sexp 1)
      (buffer-substring l (point)))))

(defun endless/ruby-copy-class-name ()
  (interactive)
  (save-excursion
    (let ((name nil)
          (case-fold-search nil))
      (skip-chars-backward (rx (syntax symbol)))
      (when (looking-at-p "\\_<[A-Z]")
        (setq name (endless/-ruby-symbol-at-point)))
      (while (ignore-errors (backward-up-list) t)
        (when (looking-at-p "class\\|module")
          (save-excursion
            (forward-word 1)
            (skip-chars-forward "\r\n[:blank:]")
            (setq name (if name
                           (concat (endless/-ruby-symbol-at-point) "::" name)
                         (endless/-ruby-symbol-at-point))))))
      (kill-new name)
      (message "Copied %s" name))))

;;; Personal Bindings

(global-set-key (kbd "C-S-o") (kbd "C-p C-e C-m"))
(global-set-key (kbd "C-o") (kbd "C-e C-m"))
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-=") 'iwb)
(global-set-key (kbd "C-+") 'delete-trailing-whitespace)
(global-set-key (kbd "C-q") 'yas-expand)
(global-set-key (kbd "C-x r p") (kbd "M-m C-SPC C-e M-w SPC = SPC C-y M-m @ C-n"))
(global-set-key (kbd "C-S-s") 'sp-splice-sexp)
(global-set-key (kbd "C-c r 3 T") (kbd "C-c , 4 t"))

(define-key ruby-mode-map (kbd "C-c t") #'endless/toggle-ruby-block)
(define-key ruby-mode-map (kbd "C-c w") #'endless/ruby-copy-class-name)
(define-key ruby-mode-map (kbd "C-c f") #'endless/ruby-define-method)


;;; Rubocop
(global-set-key (kbd "C-)") 'rubo_current)
(global-set-key (kbd "C-c w a")  'rubocop-autocorrect-project)

;; for keyboard with use / in Alt-gt + w

(global-set-key (kbd "C-;") 'undo)
(global-set-key (kbd "C-u") 'undo)

;;; Expand Region

(require 'expand-region)
(global-set-key (kbd "M-2") 'er/expand-region)

;;; Execute System Path (zsh or bash)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; ERB
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))
        ))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (global-set-key (kbd "C-c C-v") 'zencoding-expand-line)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)


;;; Byebug inside rspec
;;; use C-x C-q to enable
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;;; Git

(global-git-gutter-mode +1)

;;; Helm
(global-set-key (kbd "C-c C-b") 'helm-buffers-list)

;;; Eletric () and ""
;; (electric-quote-mode)
;; (electric-pair-mode)

;;; Rbenv
(require 'rbenv)
(global-rbenv-mode)

;;; Avy rulez

(define-key global-map (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-c C-w") 'avy-goto-char-timer)
(global-set-key (kbd "C-c w") 'avy-goto-char-timer)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

;;; JS
(setq js-indent-level 2)

;;; SCSS
(add-hook 'scss-mode-hook
          '(lambda()
             (setq tab-width 2)
             (setq indent-tabs-mode nil)))

;;; We Want accents
;; if dont work, run your emacs with: "env XMODIFIERS= emacs"
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(put 'upcase-region 'disabled nil)

;;; Zencoding rulez
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;; .arb is ruby!
(add-to-list 'auto-mode-alist '("\\.arb\\'"          . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.axlsx\\'"          . ruby-mode))

;; Javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;;; .es6 is javascript!
(add-to-list 'auto-mode-alist '("\\.es6\\'"          . js2-mode))

;;; Ace Window
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x w u") 'winner-undo)
(global-set-key (kbd "C-x w r") 'winner-redo)
(global-set-key (kbd "C-x w s") 'ace-swap-window)

(winner-mode t)

;;; Edit Server

(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start)
  (define-key edit-server-edit-mode-map
    [remap save-and-close-client]
    #'edit-server-done)
  (add-hook 'edit-server-done-hook #'suspend-frame)
  (add-hook 'edit-server-start-hook #'raise-frame)
  (add-to-list 'edit-server-url-major-mode-alist
               '("github\\.com" . markdown-mode))
  (add-to-list 'edit-server-url-major-mode-alist
               '(".*" . markdown-mode) 'append))

;;; Anottate scroll
(defun post-command-recenter-once ()
  (recenter 10)
  (remove-hook 'post-command-hook #'post-command-recenter-once))

(defun ruby-goto-model-start ()
  "Move point to the start of the current model."
  (when (and (string-match "app/models/.*\\.rb" (buffer-file-name))
             (search-forward-regexp "^class .* < ApplicationRecord" nil 'noerror))
    (goto-char (match-beginning 0))
    (add-hook 'post-command-hook #'post-command-recenter-once)))

(add-hook 'find-file-hook #'ruby-goto-model-start)

;;; Smartparens
(require 'smartparens-config)
(require 'smartparens-ruby)
(add-hook 'ruby-mode-hook #'smartparens-mode)

(smartparens-global-mode)


(global-set-key (kbd "C-S-k") 'sp-kill-hybrid-sexp)
;;; Reload Buffer
(global-set-key (kbd "C-x r <RET>") #'msc/revert-buffer-noconfirm)

(defun msc/revert-buffer-noconfirm ()
  "Call `revert-buffer' with the NOCONFIRM argument set."
  (interactive)
  (revert-buffer nil t))

;;; Indent tabs
(setq-default indent-tabs-mode nil)

;;; Highlight current light
(global-hl-line-mode +1)

;;; hide dired detail
(add-hook 'dired-mode-hook
          (lambda () (dired-hide-details-mode +1)))

(delete-selection-mode 1)

;;; .emacs ends here
(put 'downcase-region 'disabled nil)

(require 'spaceline-config)
(spaceline-spacemacs-theme)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-center-content t)

(setq dashboard-items '((recents  . 10)
                        (bookmarks . 10)
                        (projects . 5)))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(require 'all-the-icons)

(beacon-mode 1)

(counsel-mode 1)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)

;;; Drag Stuff
(drag-stuff-global-mode)
(global-set-key (kbd "M-P") 'drag-stuff-up)
(global-set-key (kbd "M-N") 'drag-stuff-down)
(global-set-key [M-S-up] 'drag-stuff-up)
(global-set-key [M-S-down] 'drag-stuff-down)

(all-the-icons-ivy-setup)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq all-the-icons-ivy-buffer-commands '())

(setq projectile-completion-system 'helm)
(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (doom-molokai)))
 '(custom-safe-themes
   (quote
    ("f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" default)))
 '(package-selected-packages
   (quote
    (flycheck-pos-tip flycheck-popup-tip zencoding-mode zenburn-theme yasnippet-snippets yasnippet-classic-snippets yaml-mode xref-js2 websocket web-mode textmate-to-yas spacemacs-theme spaceline solarized-theme smartparens rubocop rspec-mode robe restclient request rbenv railscasts-reloaded-theme projectile-rails pacmacs oauth2 nimbus-theme multi monokai-theme monokai-pro-theme molokai-theme moe-theme markdown-mode magit linum-relative kaolin-themes js2-refactor hungry-delete highlight-indent-guides helm-projectile helm-ag gruvbox-theme grandshell-theme git-gutter flycheck feature-mode expand-region exec-path-from-shell emojify edit-server drag-stuff dracula-theme doom-themes dashboard darktooth-theme counsel company-tern color-theme-sanityinc-tomorrow clues-theme circe busybee-theme beacon ample-theme all-the-icons-ivy all-the-icons-dired alert afternoon-theme ace-window)))
 '(tool-bar-mode nil))

;; Other configs
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(save-place-mode 1)
(global-linum-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
