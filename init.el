(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; My packages
(defvar my-packages '(
                      ace-jump-mode
                      ack-and-a-half
                      auctex
                      bookmark+
                      crosshairs ;; Used by bookmark+
                      deft
                      dired+
                      fill-column-indicator
                      go-mode
                      helm
                      ibuffer-vc
                      iedit
                      key-chord
                      markdown-mode
                      multi-term
                      nav
                      occidental-theme
                      pcmpl-args
                      rainbow-mode
                      sauron
                      starter-kit
                      switch-window
                      textile-mode
                      undo-tree
                      workgroups
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/erik/.emacs.d/bookmarks"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
