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
;; wrapp lines
(global-visual-line-mode 1)  ; Enable visual-line-mode globally
(setq-default word-wrap t)    ; Enable word wrapping
(setq visual-line-fringe-indicators '(nil . nil))  ; Disable continuation arrows



;;; Colouring 'man' pages =====
;;  Add the follows to your init file and use M-x man or the man command in
;;  eshell to view man pages: (the colors fits the wombat theme; you can change them for yourself)
(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "orange red")
(set-face-attribute 'Man-underline nil :inherit 'underline :foreground "forest green")

;;Or to be theme agnostic:
;(require 'man)
;(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
;(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t
;;; =====

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
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
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
  :custom
  (evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

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

;;;  === Custom key bindings ====
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

;;;  =============================

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
;(set-face-attribute 'default nil :height 115)
;(add-to-list 'default-frame-alist '(font . "Iosevka-12" ))
;(add-to-list 'default-frame-alist '(font . "Cascadia Mono 12" ))
;(set-face-attribute 'default t :font "Cascadia Mono" )
;(add-to-list 'default-frame-alist '(font . "Liberation Mono 10" ))
;(set-face-attribute 'default t :font "Liberation Mono" )
;(set-face-attribute 'default nil :font "0xProto Nerd Mono 10" )
(set-face-attribute 'default nil :font "Cascadia Code" )

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
   '(default ((t (:font "0xProto Nerd Font-10"))))  ; Ensure default font for markdown
   '(markdown-header-face ((t (:inherit font-lock-type-face :weight regular :family "default"))))
   '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
   '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
   '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2))))))

;;; Magit
;(use-package magit
  ;:ensure t)
;; Because Magit on Windows is pain so install only on Linux
(use-package magit
  :if (not (eq system-type 'windows-nt))
  :ensure t)

;;; Ido Mode
(use-package ido
  :config
  (ido-mode t)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-all-frames nil)
  )

;;; --- Company ---
(use-package company
  :init
  (global-company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  :ensure t
  :config
  (company-mode)
  (setq company-idle-delay 0.2)
  (setq minimum-prefix-length 2)
  (setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
  (add-to-list 'company-backends 'company-c-headers) ;; Backend for header files
  (add-to-list 'company-backends 'company-elisp)
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    ;; remove
    (setq company-backends (delete 'company-clang company-backends))
    ;; add
    ;;(add-to-list 'company-backends 'company-dabbrev)
    )
  
  :bind (:map company-search-map  
              ("C-t" . company-search-toggle-filtering)
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              :map company-active-map
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)))

;;; Ya Snippets
(use-package yasnippet                  ; Snippets
  :ensure t
  :config
  ;; Set custom snippets path based on the system type
  (setq yas-snippet-dirs
        (list (if (eq system-type 'windows-nt)
                  "c:\\dotfiles\\snippets"
                "/home/krumm/dotfiles/snippets")))

  ;; Configure yasnippet settings
  (setq yas-verbosity 1)                ; No need to be so verbose
  (setq yas-wrap-around-region t)       ; Wrap snippets around region

  ;; Reload snippets after setting paths
  (yas-reload-all)

  ;; Enable yasnippet globally
  (yas-global-mode))

;;(use-package yasnippet-snippets         ; Collection of snippets
  ;;:ensure t)


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

;;; Org-Mode
(use-package org
    :ensure t
    :config
    ;; For code-blocks insentation
    ;; Make sure org file code highlights correctly
    (setq org-src-fontify-natively t)
    ;;(setq org-src-tab-acts-natively t) ; TAB-ing in code-blocks
    (setq org-src-fontify-natively t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-src-tab-acts-natively t)
    (setq org-confirm-babel-evaluate nil) ; Dont ask to evaluate code
    (setq org-todo-keywords '(
    (sequence "TODO" "In Progress" "|" "Waiting" "DONE" "Completed")
    (sequence "Queue" "Working On" "On Hold" "|" "Finished" "Worked On" "Removed")))
    )

;;; Custom headings that inherit color form default
(when (eq system-type 'windows-nt)
  ;; Custom headings that inherit color from default on Windows
  (custom-set-faces
   '(org-level-1 ((t (:inherit default :height 1.6  :weight semi-bold))))
   '(org-level-2 ((t (:inherit default :height 1.4  :weight semi-bold))))
   '(org-level-3 ((t (:inherit default :height 1.2  :weight semi-bold))))
   '(org-level-4 ((t (:inherit default :height 1.1  :weight semi-bold))))
   '(org-level-5 ((t (:inherit default :height 1.05 :weight semi-bold))))))

;;; Org-Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)        ; Enable C
   (cpp . nil)    ; Disable C++
   (python . t)   ; Enable Python
   (sql . t)      ; Enable SQL
   ;; Add other languages as needed
   ))

(use-package emacsql
  :ensure t)

;;; Org-Roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory
   (if (eq system-type 'windows-nt)
       "c:\\dotfiles\\roam\\inbox"
       "/home/krumm/dotfiles/roam/inbox"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

;; Insertin NODE immediatly withou confirm minibuffer
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
						  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; Install GPTEL if not installed
(unless (package-installed-p 'gptel)
  (package-install 'gptel))

(require 'gptel)

;; Set your OpenAI API key here (or via environment variable)
;; Load secrets.el
(when (file-exists-p "~/.emacs.d/secrets.el")
  (load "~/.emacs.d/secrets.el"))

;; Assign the key to GPTEL
(setq gptel-api-key openai-api-key)

;; Set default model to GPT-4 Mini
(setq gptel-default-model "gpt-4o-mini")

;; Optional: convenient keybinding to launch GPTEL
(global-set-key (kbd "C-c g") 'gptel)

;; Optional: auto-start GPTEL buffer in comint mode
(add-hook 'gptel-mode-hook
          (lambda ()
            (setq-local comint-prompt-read-only t)))


