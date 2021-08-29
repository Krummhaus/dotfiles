;; --------------------------------------------------------------
;; INITIAL MINIMAL SETTIGS
;; --------------------------------------------------------------
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Theme on start-up
(load-theme 'wombat)

;; Line-numbers
(global-display-line-numbers-mode t)
;; Disable numbering for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-number-mode 0))))

;; Column nubers
;; (column-number-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
	("ORG" . "https://orgmode.org/elpa/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("ORG"     . 2)
        ("MELPA"        . 0)))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; Hint package
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; --------------------------------------------------------------
;; Magit - GIT
;; --------------------------------------------------------------
(use-package magit
  :ensure t)

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

;; --------------------------------------------------------------
;; EVIL - MODE --> Vim key bindings
;; --------------------------------------------------------------
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(defun xwl-jj-as-esc ()
  (interactive)
  (if (memq evil-state '(insert replace))
      (let ((changed? (buffer-modified-p)))
          (insert "j")
          (let* ((tm (current-time))
                 (ch (read-key)))
            (if (and (eq ch ?j)
                     (< (time-to-seconds (time-since tm)) 0.5))
                (save-excursion
                  (delete-char -1)
                  (evil-force-normal-state)
                  (set-buffer-modified-p changed?))
              (insert ch))))
    (call-interactively 'evil-next-line)))

(define-key evil-insert-state-map  "j" 'xwl-jj-as-esc)
(define-key evil-replace-state-map "j" 'xwl-jj-as-esc)

;; --------------------------------------------------------------
;; HELM for navigation
;; --------------------------------------------------------------
(use-package helm
  :init
    (require 'helm-config)
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t)
  :config 
    (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
    (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
    (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
    (define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
    (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
    (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
    (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
    (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
    (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
    (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-S-h") 'describe-key)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
  :ensure t)

;; --------------------------------------------------------------
;; STATUS LINE
;; --------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


;; --------------------------------------------------------------
;; Projectile
;; --------------------------------------------------------------
(unless (package-installed-p 'projectile)
  (package-install 'projectile))


;; --------------------------------------------------------------
;; PYTHON
;; --------------------------------------------------------------
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; --------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (counsel use-package ivy evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
