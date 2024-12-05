;;; Startup
(setq gc-cons-treshold (* 100 1000 1000))
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (message "Startup in %s sec with %d garbage collections"
		       (emacs-init-time "%.2f")
		       gcs-done)))

;; Startup Screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;; Get bars
;(menu-bar-mode -1)       ; Disable the menu bar
(tool-bar-mode -1)       ; Disable the tool bar
(scroll-bar-mode -1)     ; Disable the scroll bar
(setq scroll-bar-mode -1)
;; Dont make backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
;;For Emacs versions prior to 28, you must use fset: (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)
;; confirmation if a file or buffer does not exist when you use C-x C-f or C-x b.
(setq confirm-nonexistent-file-or-buffer nil)
;; Warning levels
(setq warning-minimum-level :error)
;;; Brackets
(electric-pair-mode 1)

;;; Line
;; Line Numbers
;(global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative) ; Options: 'relative, 'absolute, or nil
;; Line Wrapping
; Global
(setq-default truncate-lines t)
; In-some modes
;(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))  ;; Programming modes
;(add-hook 'text-mode-hook (lambda () (setq truncate-lines t)))  ;; Text-based modes

;;; Package Repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; Use Package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; Theme
(load-theme 'deeper-blue t)

;;; Evil Mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Evil imap jj <Esc>
(defun xwl-jj-as-esc ()
  (interactive)
  (if (memq evil-state '(insert replace))
      (let ((changed? (buffer-modified-p)))
          (insert "j")
          (let* ((tm (current-time))
                 (ch (read-key)))
            (if (and (eq ch ?j)
                     (< (time-to-seconds (time-since tm)) 0.2))
                (save-excursion
                  (delete-char -1)
                  (evil-force-normal-state)
                  (set-buffer-modified-p changed?))
              (insert ch))))
    (call-interactively 'evil-next-line)))

(define-key evil-insert-state-map  "j" 'xwl-jj-as-esc)
(define-key evil-replace-state-map "j" 'xwl-jj-as-esc)

;; Turn of Evil in dired mode
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)

;;; Custom key bindings
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-,") 'dupl-line)
(global-set-key (kbd "C-5") 'compile)
(global-set-key (kbd "C-0") 'shell-command)

;; for clang-format
(defun insert-clang-format-off ()
  "Inserts '// clang-format off' at the point."
  (interactive)
  (insert "// clang-format off"))

(defun insert-clang-format-on ()
  "Inserts '// clang-format on' at the point."
  (interactive)
  (insert "// clang-format on"))
(global-set-key (kbd "C-c f") 'insert-clang-format-off)
(global-set-key (kbd "C-c o") 'insert-clang-format-on)

;; "M-x" remap to "Shift + Space"
(global-set-key (kbd "S-SPC") 'execute-extended-command)


;;; Unicode Everywhere
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(set locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
(if (eq system-type 'windows-nt)
    'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
    'utf-8))
(prefer-coding-system 'utf-8)


;;; Font
(set-face-attribute 'default nil :height 115)
;(add-to-list 'default-frame-alist '(font . "Iosevka-12" ))
;(add-to-list 'default-frame-alist '(font . "Cascadia Mono 12" ))
;(set-face-attribute 'default t :font "Cascadia Mono" )
;(add-to-list 'default-frame-alist '(font . "Liberation Mono 10" ))
;(set-face-attribute 'default t :font "Liberation Mono" )

;;; Markdown
;; Dont show markup tags (M-x markdown-toggle-markup-hiding)
;; Use use-package to manage markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :config
  ;; Custom headings in markup
  (custom-set-faces
   '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
   '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))))
   '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))))
   '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1))))))

;;; Magit
;(use-package magit
  ;:ensure t)
;; Because Magit on Windows is pain so install only on Linux
(use-package magit
  :if (not (eq system-type 'windows-nt))
  :ensure t)

;;; Ido Mode
(ido-mode t)

;; Define the duplicate-line function
(defun dupl-line ()
  "Duplicate the current line."
  (interactive)
  (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
    (end-of-line)
    (newline)
    (insert text)))

;;; Clang Format
;; Load clang-format
;(require 'clang-format)
(if (not (eq system-type 'windows-nt))
    (require 'clang-format))

;; Function to run clang-format on save
(defun clang-format-on-save ()
  "Format the current buffer with clang-format on save."
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'objc-mode 'proto-mode)
    (clang-format-buffer)))

;; Add the hook to the before-save-hook
(add-hook 'before-save-hook 'clang-format-on-save)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit markdown-mode evil-collection gruber-darker-theme evil-org evil-leader)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
