;; Do not make files with ~ and ##
(setq make-backup-files nil)

;; Cygwin as a Emacs shell
(setq shell-file-name "c:/cygwin64/bin/bash.exe")
(setq explicit-shell-file-name shell-file-name)

;; Display line number when programming
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Evil Mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>"))

  (use-package evil-org
    :ensure t
    :config
    (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
    (add-hook 'org-mode-hook (lambda () (evil-org-mode))))
  )

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init)

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

;; Org-Roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-diretory "c:\\Users\\kalina\\org-roam")
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

;; Org-Mode
  (use-package org
    :ensure t
    :config
    ;; For code-blocks insentation
    ;; Make sure org file code highlights correctly
    (setq org-src-fontify-natively t)
;;    (setq org-src-tab-acts-natively t) ; TAB-ing in code-blocks
    (setq org-src-fontify-natively t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-src-tab-acts-natively t)
    (setq org-confirm-babel-evaluate nil) ; Dont ask to evaluate code
    (setq org-todo-keywords '(
;;(sequence "TODO" "In Progress" "|" "Waiting" "DONE" "Completed")
			      (sequence "Queue" "Working On" "On Hold" "|" "Finished" "Worked On" "Removed")))

    (evil-leader/set-key-for-mode 'org-mode
      "m i" 'org-clock-in
      "m C" 'org-toggle-checkbox
      "m o" 'org-clock-out
      "m c" 'org-clock-jump-to-current-clock
      "m d" 'org-clock-display
      "m e" 'org-set-effort
      "m p" 'org-pomodoro
      "m t" 'org-set-tags-command
      "m m" 'org-clock-modify-effort-estimate
      "m s" 'org-schedule)

    ;; ;; Org Agenda stuff
;;    (setq org-agenda-files '("~/nextcloud/emacs/org/archive/work.org"))

    (defun org-code (start end)
      "Wraps selected text with org code tags"
      (interactive "r")
      (let ((selected-text (buffer-substring (mark) (point)))
	    (db-name (read-from-minibuffer "Language[common-lisp]: ")))
	(when (eq db-name "")
	  (setf db-name "common-lisp"))
	(kill-region start end)
	(insert (format "#+BEGIN_SRC %s \n%s \n#+END_SRC" db-name selected-text))))
)

;; TABing inside of code blocks
;; TODO

;;Org-Babel
(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((C . t)
	 (cpp . nil)
	   ;; other languages..
))

(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(setq org-src-block-faces '(("C" (:background "#EEE2FF"))
                            ("python" (:background "#E5FFB8"))))

;; Ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; Ivy Keybindings
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

;; Swiper
(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper)
  )

;; Counsel
(use-package counsel 
  :ensure t
  :config 
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package yasnippet
    :config
    (setq yas-snippet-dirs '("c:\\Users\\kalina\\org-roam\\yasn"))
    (yas-global-mode 1))

;; Magit, Git  version control
(use-package magit
  :ensure t)
