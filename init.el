;; Minimal init.el
(defvar user (getenv "USER"))
(package-initialize)
;; Change to the path where you cloned the config to
(org-babel-load-file (format "/home/%s/.emacs.d/config.org" user))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ob-ipython elpy json-mode yaml-mode magit smartparens aggressive-indent auto-complete aggressive-fill-paragraph wc-mode markdown-mode htmlize org-pomodoro counsel swiper ivy powerline-evil evil-org evil-indent-textobject evil-surround evil-leader evil auto-compile use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
