;; New line like vim
(global-set-key (kbd "C-o") (kbd "C-e C-m"))
(global-set-key (kbd "C-S-o") (kbd "C-p C-e C-m"))

;; Join line
(global-set-key (kbd "C-c C-j") 'join-line)

;; New undo alternatives

(global-set-key (kbd "C-;") 'undo)
(global-set-key (kbd "C-u") 'undo)

;; Expand Region
(use-package expand-region
  :init
  (global-set-key (kbd "M-2") 'er/expand-region)
  )

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip)
(use-package flycheck-popup-tip
  :config
  (with-eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))
  (eval-after-load 'flycheck
    (if (display-graphic-p)
        (flycheck-pos-tip-mode)
      (flycheck-popup-tip-mode)))
  )

;; Ace window
(use-package ace-window
  :init
  (global-set-key (kbd "M-o") 'ace-window)
  (global-set-key (kbd "C-x w u") 'winner-undo)
  (global-set-key (kbd "C-x w r") 'winner-redo)
  (global-set-key (kbd "C-x w s") 'ace-swap-window)
  )

;; Company - Auto complation

(use-package company
  :init
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends))

  (global-company-mode)
  )

(use-package smartparens
  :init
  (smartparens-global-mode)
  :config
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(loop for (key . val) in pairs
               collect
               `(defun ,(read (concat
                               "wrap-with-"
                               (prin1-to-string key)
                               "s"))
                    (&optional arg)
                  (interactive "p")
                  (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`")))

  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp))


  (require 'smartparens-ruby)
  (require 'smartparens-config)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode))

(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c g") 'magit-status)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  )


(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (eval-after-load 'rspec-mode
    '(rspec-install-snippets))
  (global-set-key (kbd "C-q") 'yas-expand)
  )

(use-package yasnippet-snippets)
(use-package yasnippet-classic-snippets
  :after
  (yas-reload-all)
  )
