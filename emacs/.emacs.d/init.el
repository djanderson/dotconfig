;; Douglas Anderson's emacs.d/init.el file

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

(use-package treemacs
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

(use-package yasnippet)

(use-package lsp-mode
  :hook (prog-mode . lsp)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
(use-package company-lsp :commands company-lsp)
(use-package dap-mode
  :config
  (require 'dap-python)
  (require 'dap-lldb)                   ; c / c++ / rust
  )

(use-package cquery
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'cquery) (lsp)))
  :config
  (setq cquery-cache-dir "/home/dja/src/cquery/.cquery_cached_index")
  (setq cquery-executable "/home/dja/src/cquery/build/release/bin/cquery"))

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
  (add-hook 'rust-mode-hook
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

(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

; https://github.com/tuhdo/semantic-refactor
(use-package srefactor
  :config
  (require 'srefactor-lisp)
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
  (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
  (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
  (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer))

(use-package company
  :config
  (progn
    (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
    (setq company-idle-delay 0)))

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
         ("C-x C-g b" . magit-blame))
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :bind
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error)
  :config
  (progn
    (global-flycheck-mode)))

(use-package blacken)
(use-package isortify)

(use-package elpy
  :config
  (elpy-enable)
  (add-hook 'python-mode-hook
            (lambda ()
              (elpy-mode t)))
  (setq elpy-rpc-python-command "python3")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'blacken-mode)
  (add-hook 'elpy-mode-hook 'isortify-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package cargo)

(use-package rust-mode
  :config
  ;; set to t when rustfmt supports vertically aligning matches/enums
  (setq rust-format-on-save nil)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-cask
  :commands flycheck-cask-setup
  :config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup)))

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

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
 '(company-tooltip-align-annotations f)
 '(custom-enabled-themes (quote (wombat)))
 '(elpy-company-post-completion-function (quote ignore))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(elpy-project-ignored-directories
   (quote
    (".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" ".eggs")))
 '(elpy-test-discover-runner-command (quote ("python3" "-m" "pytest")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(package-selected-packages
   (quote
    (shell-pop centaur-tabs treemacs-magit treemacs dap-mode: cquery lsp-mode protobuf-mode cmake-mode company-c-headers flycheck-clang-analyzer yafolding go-mode auto-complete isortify blacken web-mode ido-completing-read+ smex flx-ido ido-vertical-mode srefactor ac-c-headers cider clojure-mode projectile markdown-preview-mode markdown-mode arduino-mode flycheck-rust yaml-mode magit company-racer use-package cargo rjsx-mode js2-mode paredit elpy company-go racer company epl flycheck sbt-mode scala-mode expand-region toml-mode spinner rust-mode queue package-utils ggtags fill-column-indicator exec-path-from-shell ensime edit-server better-defaults))))
 '(subword-mode t t)
 '(visible-bell (quote top-bottom))

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
  (push (cons "shell" display-buffer--same-window-action) display-buffer-alist)
  :bind
  ("<f4>" . shell-pop)
  :config
  (setq shell-pop-internal-mode-buffer "shell")
  (setq shell-pop-term-shell "/bin/bash")
  (setq shell-pop-full-span nil)
  (setq shell-pop-universal-key "<f4>"))
;; C-# f4 to open # tabs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cleanup trailing whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-M-j") 'join-line)

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

(provide 'init)
;;; init.el ends here
