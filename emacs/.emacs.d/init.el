;; Douglas Anderson's emacs.d/init.el file

;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 scroll-error-top-bottom t
 show-paren-delay 0.5
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
(electric-indent-mode t)
(electric-layout-mode t)
(electric-pair-mode t)
(line-number-mode t)
(show-paren-mode t)

;; setup the package manager
(require 'package)
(setq
 package-archives '(("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install 'use-package)
(require 'use-package)

(setq use-package-always-ensure t) ;; auto install all packages

(use-package better-defaults
  :demand)

;; package settings
(use-package ensime
  :pin melpa-stable)

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(require 'ensime-expand-region)

;; inherit PATH var from shell (https://github.com/purcell/exec-path-from-shell)
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variable '("PATH" "MANPATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))

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
              (fci-mode t))))


;; parse cmake files to allow smarter syntax checking/autocomplete, etc
(use-package cpputils-cmake
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (if (derived-mode-p 'c-mode 'c++-mode)
                  (cppcm-reload-all)))))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook
          (lambda ()
            (yas-minor-mode)
            (company-mode)
            (ensime-mode)
            (scala-mode:goto-start-of-code)
            (subword-mode))))

(use-package go-mode
  :bind
  ("M-." . godef-jump)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'subword-mode))

(use-package company-go
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode))))

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package yaml-mode)

(use-package racer
  :config
  ;;(setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "~/srcbuilds/rust/src")
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package magit
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("C-x g" . magit-status)
         ("C-x C-g b" . magit-blame)))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :bind
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error)
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (flycheck-mode t)))
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook
            (lambda ()
              (flycheck-mode t)))
  (add-hook 'go-mode-hook
            (lambda ()
              (flycheck-mode t)))
  (add-hook 'octave-mode-hook
            (lambda ()
              (flycheck-mode t))))

(use-package elpy
  :bind
  ("M-n" . elpy-flymake-next-error)
  ("M-p" . elpy-flymake-previous-error)
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (elpy-use-ipython "ipython3"))

(use-package flycheck-cask
  :commands flycheck-cask-setup
  :config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup)))

;; make mac key placement match PC
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; turn off toolbar and menubar and scrollbar
(when window-system
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode 0))
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode 0))
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
  (tooltip-mode nil))

;; avoid garbage collection up to 10M (default is only 400k)
(setq-default gc-cons-threshold 10000000)

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
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (elpy company-go yaml-mode racer racer-mode cargo-mode company dash epl flycheck git-commit magit-popup sbt-mode scala-mode with-editor yasnippet expand-region use-package toml-mode spinner rust-mode queue package-utils magit ggtags flycheck-rust flycheck-pyflakes fill-column-indicator exec-path-from-shell ensime edit-server cpputils-cmake better-defaults)))
 '(visible-bell (quote top-bottom)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PAGER" "cat")

(defvar local-shells
  '("*shell0*" "*shell1*" "*shell2*" "*shell3*"))

;; run a few shells.
(defun start-shells ()
  (mapcar 'shell local-shells))

;;(defun fix-shell ()
;;  "Sometimes the input area of a shell buffer goes read only. fix that."
;;  (interactive)
;;  (let ((inhibit-read-only t))
;;    (comint-send-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-prettify-symbols-mode +1)

(add-hook 'scala-mode-hook
          (lambda ()
            (setq prettify-symbols-alist scala-prettify-symbols-alist)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; Cleanup trailing whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Use sh-mode for Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile" . sh-mode))

;; Use octave-mode for Matlab .m files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; C
;;(setq-default c-basic-offset 4 c-default-style "k&r")
(setq-default c-basic-offset 2 c-default-style "k&r")

;; easy switching between header and implemetation files
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

;; turn on pending delete (when a region is selected, typing replaces it)
(delete-selection-mode t)

;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)

;; turn off "yes" / "no" full word prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; use utf-8! details:
;; http://www.masteringemacs.org/articles/2012/08/09/working-coding-systems-unicode-emacs/
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

;; this is suspend-frame by default, ie minimize the window if graphical
(global-unset-key [(control z)])

(start-shells)

(provide 'init)
;;; init.el ends here
