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
(defun my/org-electric-pair-inhibit (char)
  (or (and (eq major-mode 'org-mode)
           (char-equal char ?<))
      (electric-pair-default-inhibit char)))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate
                        #'my/org-electric-pair-inhibit)))
;; wrapp lines
(global-visual-line-mode 1)  ; Enable visual-line-mode globally
(setq-default word-wrap t)    ; Enable word wrapping
(setq visual-line-fringe-indicators '(nil . nil))  ; Disable continuation arrows
;; Turn-off sounds
(setq visible-bell t)

;; Return to recent edit file
(recentf-mode 1)

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
(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)

;;;  === Custom key bindings ====
;(global-set-key (kbd "C-,") 'dupl-line)
(global-set-key (kbd "C-<return>") 'compile)
(global-set-key (kbd "C-S-c") 'kill-ring-save) ;; Copy
(global-set-key (kbd "C-S-v") 'yank)           ;; Paste

;;; ===== prog languages =====
;;; c-mode
(setq c-default-style "stroustrup"
          c-basic-offset 4)

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

;;; Golang
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)           ;; tabs display as 4 spaces
            (setq indent-tabs-mode t)    ;; use real tabs, not spaces
            (add-hook 'before-save-hook 'gofmt-before-save nil 'local))) ;; format on save

(setq gofmt-command "goimports")  ;; use goimports instead of gofmt

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            (add-hook 'before-save-hook 'gofmt-before-save nil 'local)))

;;;  =============================

;;; UTF-8 Everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; needed for clipboard on Windows
   'utf-8))
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Make sure subprocesses (shell, compile, Python) use UTF-8
(add-to-list 'process-coding-system-alist '(".*" . (utf-8 . utf-8)))
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "PYTHONIOENCODING" "utf-8")


;;; Font
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :height 110)
(set-face-attribute 'default nil :height 100))

;(add-to-list 'default-frame-alist '(font . "Iosevka-12" ))
;(add-to-list 'default-frame-alist '(font . "Cascadia Mono 12" ))
;(set-face-attribute 'default t :font "Cascadia Mono" )
;(add-to-list 'default-frame-alist '(font . "Liberation Mono 10" ))
;(set-face-attribute 'default t :font "Liberation Mono" )
;(set-face-attribute 'default nil :font "0xProto Nerd Mono 10" )
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Cascadia Code"))

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
   '(markdown-header-face ((t (:inherit font-lock-type-face :weight semi-bold ))))
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
  ;; Prevent ido from searching outside the current directory
  (setq ido-auto-merge-work-directories-length -1)

  ;; Don't guess filenames from elsewhere
  (setq ido-use-filename-at-point nil)

  ;; Limit ido to current dir and subdirs only
  (setq ido-case-fold t)
  (setq ido-enable-prefix nil)
  (setq ido-enable-regexp nil)
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
    ;; Set margins in Org mode
    (add-hook 'org-mode-hook
          (lambda ()
            (setq-local left-margin-width 2
                        right-margin-width 2)
            (set-window-buffer nil (current-buffer)))) ;; refresh margins
    )

;; TAB in org-mode oveeriding evil-mode
(defun my-tab ()
  "Custom TAB behavior: Org-mode cycles, otherwise evil-jump-forward."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-cycle)
    (evil-jump-forward)))

;; Remap TAB in evil-motion-state-map to our wrapper
(define-key evil-motion-state-map (kbd "TAB") #'my-tab)

;; Org Tags
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face ((t (:inherit font-lock-type-face :weight semi-bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2))))
 '(org-level-1 ((t (:foreground "#F099FF" :weight bold :height 1.35))))
 '(org-level-2 ((t (:foreground "#99A8FF" :weight semi-bold :height 1.2))))
 '(org-level-3 ((t (:foreground "#A8FF99" :weight semi-bold :height 1.1))))
 '(org-level-4 ((t (:foreground "#DBFF99" :weight semi-bold :height 1.05))))
 '(org-level-5 ((t (:foreground "#FFF099" :weight semi-bold :height 1.0))))
 '(org-tag ((t (:inherit shadow :weight normal :slant italic)))))

;;; Custom headings that inherit color form default
;; https://htmlcolorcodes.com/color-wheel/

 

;;; Org-Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)        ; Enable C
   ;(cpp . t)    ; Disable C++
   (python . t)   ; Enable Python
   (sql . t)      ; Enable SQL
   (js . t)      ; Enable JS
   ;; Add other languages as needed
   ))

(require 'org-tempo) ; Make sure org-tempo is loaded

(add-to-list 'org-structure-template-alist
             '("py" . "src python")) ; Shortcut: <py TAB -> #+BEGIN_SRC python ... #+END_SRC

(add-to-list 'org-structure-template-alist
             '("cc" . "src C"))       ; Shortcut: <cc TAB -> #+BEGIN_SRC C ... #+END_SRC

(add-to-list 'org-structure-template-alist
             '("sql" . "src sql"))   ; Shortcut: <sql TAB -> #+BEGIN_SRC sql ... #+END_SRC

;;; Babel-Python for Linux
(when (eq system-type 'gnu/linux)
  (setq org-babel-python-command "python3"))

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

;;; Install GPTEL if not installed
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
;; Configure gptel to use org-mode syntax
(setq gptel-org-mode t)
;; Load gptel
;(add-hook 'org-mode-hook (lambda () (gptel-mode 1)))
;; Optional: auto-start GPTEL buffer in comint mode
;(add-hook 'gptel-mode-hook
          ;(lambda ()
            ;(setq-local comint-prompt-read-only t)))

;; Gptel prompts
(gptel-make-preset 'gpt4coding                       ;preset name, a symbol
  :description "A preset optimized for coding tasks" ;for your reference
  :backend "ChatGPT"                     ;gptel backend or backend name
  :model 'gpt-4o-mini
  :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
  :tools '("read_buffer" "modify_buffer")) ;gptel tools or tool names


;;; Email client GNUS
(setq gnus-select-m(setq gnus-select-method
      '(nnimap "tkpgeo"
               (nnimap-address "budejovice.tkpgeo.cz")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnimap-user "kalina")))
      )
;; Sort threads by date (most recent first)
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)))

;; Sort articles by date
(setq gnus-thread-sort-functions 'gnus-thread-sort-by-date)

;; Optional: show newest articles at the top in summary buffer
(setq gnus-article-sort-functions
      '(gnus-article-sort-by-date))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Request package to API calls
(use-package request
  :ensure t)

;;; RFC specification
(use-package rfc-mode
  :ensure t
  :config
  ;; Different paths per OS
  (setq rfc-mode-directory
        (cond
         ((eq system-type 'windows-nt) "c:/rfc_emacs")
         ((eq system-type 'gnu/linux) "/home/krumm/rfc_emacs")
         ((eq system-type 'darwin) "/Users/krumm/rfc_emacs")))

  ;; Optional: always fetch newer RFCs if available
  (setq rfc-mode-update-files t)

;; Download and use the official index
  (setq rfc-mode-index-update t)   ;; refresh index if stale
  (setq rfc-mode-index-filename
        (expand-file-name "rfc-index.txt" rfc-mode-directory))

  ;; Optional: keybindings
  :bind
  (("C-c r n" . rfc-mode)          ;; open by number
   ("C-c r s" . rfc-mode-browse))) ;; search by title/abstract  ;; Optional: shortcut keybinding

;; Optional: make Gnus fetch headers efficiently
(setq gnus-fetch-old-headers t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rfc-mode go-mode which-key gnuplot-mode gnuplot yasnippet org-roam markdown-mode magit gptel evil company)))
