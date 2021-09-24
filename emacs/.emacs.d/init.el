;;; init.el --- Douglas Anderson's emacs.d/init.el file

;;; Code:

;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 scroll-error-top-bottom t
 show-paren-delay 0
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4
 fill-column 79)

;; modes
(electric-indent-mode t)
(electric-pair-mode t)
(line-number-mode t)
(show-paren-mode t)
(global-subword-mode t)

;; setup the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install 'use-package)
(require 'use-package)

(setq use-package-always-ensure t) ;; auto install all packages

(use-package better-defaults
  :demand)

(use-package tex :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

;; TODO: want to use SPC TAB for 'mode-line-other-buffer
(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "jk" 'mode-line-other-buffer))

(use-package treemacs
  :config
  (setq treemacs-is-never-other-window t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-magit
  :after treemacs)

(use-package flycheck
  :ensure
  :bind
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error)
  :config
  (progn
    (global-flycheck-mode)))

(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :config
  (ido-mode 1)
  (ido-vertical-mode 1)
  (ido-everywhere 1)
  (setq ido-use-faces t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))


;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(use-package projectile)

(use-package company
  :ensure
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
          ("C-n". company-select-next)
          ("C-p". company-select-previous)
          ("M-<". company-select-first)
          ("M->". company-select-last)))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :config (setq lsp-metals-treeview-show-when-views-received t))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package toml-mode :ensure)

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package lsp-mode
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.5)
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 1024 1024))
  (treemacs-space-between-root-nodes nil)
  (lsp-headerline-breadcrumb-enable t)
  :hook ((python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (scala-mode . lsp)
         (web-mode . lsp)
         (typescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
    :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
         :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))

;; which-key integration
(use-package which-key
    :config
    (which-key-mode))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; yafolding default keymap:
;;   (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
;;   (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
;;   (define-key map (kbd "<C-return>")   #'yafolding-toggle-element)
(use-package yafolding
  :config
  (add-hook 'prog-mode-hook
            (lambda () (yafolding-mode))))

(use-package fill-column-indicator
  :demand
  :config
  (setq fill-column 78)
  (setq fci-rule-column 80)
  (setq fci-rule-color "dimgray")
  (setq fci-rule-width 2)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (fci-mode t)))
  (add-hook 'python-mode-hook
            (lambda ()
              (fci-mode t)))
  (add-hook 'scala-mode-hook
            (lambda ()
              (fci-mode t)))
  (add-hook 'rustic-mode-hook
            (lambda ()
              (fci-mode t)))
  (add-hook 'go-mode-hook
            (lambda ()
              (fci-mode t)))
  (add-hook 'octave-mode-hook
            (lambda ()
              (fci-mode t)))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (fci-mode t))))

;; compat for company-mode and fci-mode
;; https://github.com/company-mode/company-mode/issues/180
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "RoyalBlue4"))))
 '(company-scrollbar-fg ((t (:background "goldenrod"))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(hl-line ((t (:background "gray20")))))

(use-package yaml-mode)

(use-package magit
  :commands magit-status magit-blame
  :bind (("C-x g" . magit-status)
         ("C-x C-g b" . magit-blame)))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package blacken)
(use-package isortify)

;; turn off toolbar and menubar and scrollbar
(when window-system
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode 0))
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode 0))
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
  (tooltip-mode nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(comint-buffer-maximum-size 10000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 1000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(company-echo-truncate-lines t)
 '(company-frontends
   (quote
    (company-preview-if-just-one-frontend company-echo-metadata-frontend company-pseudo-tooltip-unless-just-one-frontend)))
 '(company-show-numbers t)
 '(company-tooltip-align-annotations nil)
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (shell-pop centaur-tabs treemacs-magit treemacs dap-mode lsp-mode protobuf-mode cmake-mode company-c-headers yafolding go-mode auto-complete isortify blacken web-mode ac-c-headers projectile markdown-preview-mode markdown-mode arduino-mode yaml-mode magit use-package rjsx-mode js2-mode paredit company-go company epl sbt-mode scala-mode expand-region toml-mode spinner queue package-utils ggtags fill-column-indicator exec-path-from-shell ensime edit-server better-defaults)))
 '(visible-bell (quote top-bottom)))

(global-hl-line-mode 1)

(set-face-attribute  'mode-line
                     nil
                     :foreground "gray100"
                     :background "gray30"
                     :box '(:line-width 1 :style released-button))
(set-face-attribute  'mode-line-inactive
                     nil
                     :foreground "gray30"
                     :background "gray20"
                     :box '(:line-width 1 :style released-button))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-set-modified-marker t)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'shell-mode)
       "Shells")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t" . centaur-tabs-counsel-switch-group))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PAGER" "cat")

(use-package shell-pop
  :init
  ; https://github.com/kyagi/shell-pop-el/issues/51#issuecomment-297470855
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
  :bind
  ("C-t" . shell-pop)
  :config
  (setq shell-pop-term-shell "/bin/bash")
  (setq shell-pop-internal-mode-buffer "shell")
  (setq shell-pop-full-span t)
  (setq shell-pop-autocd-to-working-dir nil)
  ;; C-x o shouldn't cycle to shells, use C-t to jump directly
  (setq shell-pop-in-after-hook
        (lambda nil
          (set-window-parameter
           (get-buffer-window
            (buffer-name (current-buffer))) 'no-other-window t))))
;; C-# C-t to open # tabs

(defun remote-shell (connection)
  "Pops a shell for a remote CONNECTION over tramp."
  (interactive "suser@host: ")
  (let ((default-directory (concat "/ssh:" connection ":")))
    (shell-pop shell-pop-last-shell-buffer-index)))

;; Binding shell-pop to C-t seems to also bind C-S-t, so revert that
(global-set-key (kbd "C-S-t") 'transpose-chars)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cleanup trailing whitespace before save
;(add-hook 'before-save-hook 'whitespace-cleanup)

;; Use sh-mode for Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile" . sh-mode))

;; Use octave-mode for Matlab .m files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; C

;; linux hacking:
;; (setq-default
;;  indent-tabs-mode 1
;;  tab-width 8
;;  c-basic-offset 8
;;  c-default-style "linux"
;;  fill-column 79)

(add-hook 'c-mode-hook
          (lambda () (setq-default
                      c-basic-offset 4
                      c-default-style "k&r"
                      comment-style 'extra-line)))

;; easy switching between header and implementation files
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;;(defun dont-indent-innamespace ()
;;   (c-set-offset 'innamespace [0]))
;;(add-hook 'c++-mode-hook 'dont-indent-innamespace)

;; Arduino (http://www.emacswiki.org/emacs-en/ArduinoSupport)

;; Javascript / Typescript

(use-package tide :ensure t)
(use-package company :ensure t)
(use-package flycheck :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://emacs.stackexchange.com/a/5583
(defun insert-color-hex (&optional arg)
  "Select a color and insert its 24-bit hexadecimal RGB format.

With prefix argument \\[universal-argument] insert the 48-bit value."
  (interactive "*P")
  (let ((buf (current-buffer)))
    (list-colors-display
     nil nil `(lambda (name)
                (interactive)
                (quit-window)
                (with-current-buffer ,buf
                  (insert (apply #'color-rgb-to-hex
                                 (nconc (color-name-to-rgb name)
                                        (unless (consp ',arg)
                                          (list (or ,arg 2)))))))))))

;; https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
;; (+ 1 2)C-c e -> 3
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(if (string-equal system-type "darwin")
    (progn
      ;; make mac key placement match PC
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      (setq visible-bell nil)
      (custom-set-variables
       '(gnutls-algorithm-priority "normal:-vers-tls1.3"))))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-M-j") 'join-line)

;; Cleanup trailing whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; turn on pending delete (when a region is selected, typing replaces it)
(delete-selection-mode t)

;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)

;; turn off "yes" / "no" full word prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; this is suspend-frame by default, ie minimize the window if graphical
(global-unset-key [(control z)])

(load-file "~/.emacs.d/custom.el")

(provide 'init)
;;; init.el ends here
