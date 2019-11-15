;;; package --- Summary:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"])
 '(beacon-color "#cc6666")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (doom-gruvbox)))
 '(custom-safe-themes
   (quote
    ("0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "ab98c7f7a58add58293ac67bec05ae163b5d3f35cddf18753b2b073c3fcd8841" "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "f11e219c9d043cbd5f4b2e01713c2c24a948a98bed48828dc670bd64ae771aa1" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" "9f9d5a42d5e637ffa3e95e5bcb4777cd66ce3dc36f85518b112f74f388e9ab59" "34dc2267328600f3065630e161a8ae59939700684c232073cdd5afbf78456670" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "332e009a832c4d18d92b3a9440671873187ca5b73c2a42fbd4fc67ecf0379b8c" "70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "6343f4d41b209fe8990e3c5f4d2040b359612ef9cd8682f1e1e2a836beba8107" "e62b66040cb90a4171aa7368aced4ab9d8663956a62a5590252b0bc19adde6bd" "0f1733ad53138ddd381267b4033bcb07f5e75cd7f22089c7e650f1bb28fc67f4" "fa477d10f10aa808a2d8165a4f7e6cee1ab7f902b6853fbee911a9e27cf346bc" "7d4340a89c1f576d1b5dec57635ab93cdc006524bda486b66d01a6f70cffb08e" "a9d67f7c030b3fa6e58e4580438759942185951e9438dd45f2c668c8d7ab2caf" "53760e1863395dedf3823564cbd2356e9345e6c74458dcc8ba171c039c7144ed" "ff829b1ac22bbb7cee5274391bc5c9b3ddb478e0ca0b94d97e23e8ae1a3f0c3e" "11e0bc5e71825b88527e973b80a84483a2cfa1568592230a32aedac2a32426c1" "51043b04c31d7a62ae10466da95a37725638310a38c471cc2e9772891146ee52" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "7f74a3b9a1f5e3d31358b48b8f8a1154aab2534fae82c9e918fb389fca776788" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "fe76f3d5094967034192f6a505085db8db6deb0e135749d9a54dc488d6d3ee2f" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "cdb3e7a8864cede434b168c9a060bf853eeb5b3f9f758310d2a2e23be41a24ae" "2878517f049b28342d7a360fd3f4b227086c4be8f8409f32e0f234d129cee925" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "f589e634c9ff738341823a5a58fc200341b440611aaa8e0189df85b44533692b" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "1d2f406a342499f0098f9388b87d05ec9b28ccb12ca548f4f5fa80ae368235b6" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(display-battery-mode t)
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(fringe-mode 10 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(helm-buffer-max-length 60)
 '(helm-window-prefer-horizontal-split nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(line-number-mode nil)
 '(linum-format " %6d ")
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(objed-cursor-color "#e74c3c")
 '(package-selected-packages
   (quote
    (beacon dashboard feature-mode busybee-theme highlight-indent-guides zenburn-theme moe-theme afternoon-theme grandshell-theme clues-theme molokai-theme spaceline git-gutter nimbus-theme ample-theme railscasts-reloaded-theme spacemacs-theme kaolin-themes gruvbox-theme monokai-theme monokai-pro-theme dracula-theme hungry-delete rubocop csv-mode restclient smartparens markdown-mode edit-server avy ace-window darktooth-theme zencoding-mode swiper ivy doom-themes solarized-theme color-theme-sanityinc-tomorrow rbenv helm-ag linum-relative yasnippet-classic-snippets magit multiple-cursors textmate-to-yas web-mode exec-path-from-shell yaml-mode company company-mode robe flycheck expand-region pacmacs yasnippet-snippets yasnippet rspec-mode helm-projectile helm projectile projectile-rails)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

;;; Commentary:
;; Emacs file for Ruby on Rails Development

;;; Code:

;;; Helm

(require 'helm-config)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x b") #'helm-buffers-list)

;;; Flycheck

(global-flycheck-mode)
(global-set-key "\C-\M-j" #'flycheck-next-error)
(global-set-key "\C-\M-i" #'flycheck-previous-error)

;; Disable here if you use reek
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

(setq projectile-switch-project-action
      #'projectile-commander)

(def-projectile-commander-method ?F
  "Git fetch."
  (magit-status)
  (if (fboundp 'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-from-upstream)
    (call-interactively #'magit-fetch-current)))

;;; Run projectile fetch + robe-start
(setq inf-ruby-console-environment "development")

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

;;; uncomment this to disable startup and menu-bar
;; (menu-bar-mode -1)



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

(fset 'create_test_file
   [?\C-x ?\C-f ?\M-n ?\C-a ?\C-s ?a ?p ?p return backspace backspace backspace ?s ?p ?e ?c ?\C-e ?\C-b ?\C-b ?\C-b ?_ ?s ?p ?e ?c return])

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
(global-set-key (kbd "C-c T") 'create_test_file)
(global-set-key (kbd "C-c r 3 T") (kbd "C-c , 4 t"))

(define-key ruby-mode-map (kbd "C-c t") #'endless/toggle-ruby-block)
(define-key ruby-mode-map (kbd "C-c w") #'endless/ruby-copy-class-name)
(define-key ruby-mode-map (kbd "C-c f") #'endless/ruby-define-method)


;;; Rubocop
(global-set-key (kbd "C-)") 'rubo_current)
(global-set-key (kbd "C-c w a")  'rubocop-autocorrect-project)

;; for keyboard with use / in Alt-gt + w
(global-set-key (kbd "C-;") 'undo)

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
(global-set-key (kbd "C-c C-w") 'avy-goto-word-1)
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

;;; .es6 is javascript!
(add-to-list 'auto-mode-alist '("\\.es6\\'"          . javascript-mode))

;;; Ace Window
(global-set-key (kbd "M-o") 'ace-window)

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

(setq dashboard-items '((recents  . 5)
                        (projects . 5)))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(require 'all-the-icons)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(beacon-mode 1)
