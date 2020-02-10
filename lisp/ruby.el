(use-package robe
  :init
  (global-robe-mode)
  (global-set-key (kbd "C-c r '") #'robe-start)
  )

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package rspec-mode
  :config
  )

(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-add-encoding-comment-on-save nil)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package hungry-delete)

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

(define-key ruby-mode-map (kbd "C-c t") #'endless/toggle-ruby-block)
(define-key ruby-mode-map (kbd "C-c w") #'endless/ruby-copy-class-name)
(define-key ruby-mode-map (kbd "C-c f") #'endless/ruby-define-method)
(define-key ruby-mode-map (kbd "C-)") #'rubo_current)

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

  (setq web-mode-extra-auto-pairs
	'(("erb"  . (("beg" "end")))
	  ("php"  . (("beg" "end")
		     ("beg" "end")))
	  ))

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  )

(add-to-list 'auto-mode-alist '("\\.arb\\'"          . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.axlsx\\'"          . ruby-mode))

(use-package yaml-mode)
