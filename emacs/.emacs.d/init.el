;; Douglas Anderson's emacs.d/init.el file
;; Additions from:
;;  - Ryan Barrett's .emacs  (https://snarfed.org/dotfiles/.emacs)
;;  - http://clojure-doc.org/articles/tutorials/emacs.html

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defvar my-packages '(better-defaults
                      exec-path-from-shell
                      fill-column-indicator
                      ggtags
                      arduino-mode
                      clojure-mode
                      cider
                      magit
                      haskell-mode
                      flycheck
                      flycheck-rust
                      flycheck-pyflakes
                      rust-mode
                      toml-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; turn off toolbar and menubar and scrollbar!
(when window-system
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode 0))
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode 0))
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
  (tooltip-mode 0))

; avoid garbage collection up to 10M (default is only 400k)
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
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(indent-tabs-mode nil)
 '(line-number-mode t)
 '(python-shell-interpreter "ipython")
 '(visible-bell (quote top-bottom)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; inherit PATH var from shell (https://github.com/purcell/exec-path-from-shell)
(require 'exec-path-from-shell)
(setq exec-path-from-shell-variable '("PATH" "MANPATH" "PYTHONPATH"))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'server)
(unless (server-running-p)
  (server-start))

(setenv "PAGER" "cat")

(defvar local-shells
  '("*shell0*" "*shell1*" "*shell2*" "*shell3*"))



;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

; interpret and use ansi color codes in shell buffers
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun make-my-shell-output-read-only (text)
  "Add output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) local-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell."
  (set (make-local-variable 'scroll-conservatively) 20))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

;; run a few shells.
(defun start-shells ()
  (interactive)
  (let ((default-directory "~")
        ;; trick comint into thinking the current window is 80 columns, since it
        ;; uses that to set the COLUMNS env var. otherwise it uses whatever the
        ;; current window's width is, which could be anything.
        (window-width (lambda () 80)))
    (mapcar 'shell local-shells)))

(defun fix-shell ()
  "Sometimes the input area of a shell buffer goes read only. This fixes that."
  (interactive)
  (let ((inhibit-read-only t))
    (comint-send-input)))

;;Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(require 'flycheck)

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

(require 'fill-column-indicator)
(setq-default fill-column 78)
(setq-default fci-rule-column 80)
(setq-default fci-rule-color "dimgray")
(setq-default fci-rule-width 2) ;; pixels

;; Cleanup trailing whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Use sh-mode for Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile" . sh-mode))

;; C
(setq-default c-basic-offset 4 c-default-style "linux")
;(setq-default c-basic-offset 2 c-default-style "linux")

;; easy switching between header and implemetation files
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (flycheck-mode t)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (fci-mode t)))


;;(defun dont-indent-innamespace ()
;;   (c-set-offset 'innamespace [0]))
;;(add-hook 'c++-mode-hook 'dont-indent-innamespace)

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-mode t)))

(add-hook 'python-mode-hook
          (lambda ()
            (fci-mode t)))

;; Rust
(add-hook 'rust-mode-hook
          (lambda ()
            (flycheck-mode t)))


(add-hook 'rust-mode-hook
          (lambda ()
            (fci-mode t)))

;; Arduino (http://www.emacswiki.org/emacs-en/ArduinoSupport)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

; https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
; (+ 1 2)C-c e -> 3
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

; Source an env file into process-environment variable
(defun source-env ()
  (interactive)
  (with-temp-buffer
    (call-process "bash" nil t nil "-c"
                  (concat "source "
                          (read-file-name "Source: " "~/dev/target/setup_env.sh")
                          "; env | grep PATH"))
    (goto-char (point-min))
    (while (not (eobp))
      (setq process-environment
            (cons (buffer-substring (point) (line-end-position))
                  process-environment))
      (forward-line 1))))

; turn on pending delete (when a region is selected, typing replaces it)
(delete-selection-mode t)

; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)

; turn off stupid "yes" / "no" full word prompts
(fset 'yes-or-no-p 'y-or-n-p)

; use utf-8! details:
; http://www.masteringemacs.org/articles/2012/08/09/working-coding-systems-unicode-emacs/
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

; show the matching paren immediately, we are in a hurry
(setq show-paren-delay 0)
(show-paren-mode t)

; allow narrowing
(put 'narrow-to-region 'disabled nil)

;; this is suspend-frame by default, ie minimize the window if graphical
(global-unset-key [(control z)])

(start-shells)

(provide 'init)
;;; init.el ends here
