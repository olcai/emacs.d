(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; My packages
(defvar my-packages '(
                      ace-jump-mode
                      ack-and-a-half
                      alchemist ;; Tooling for Elixir
                      auctex
                      bookmark+
                      csharp-mode
                      crosshairs ;; Used by bookmark+
                      deft
                      dired+
                      dockerfile-mode
                      elixir-mode
                      fill-column-indicator
                      go-mode
                      helm
                      ibuffer-vc
                      iedit
                      key-chord
                      ledger-mode
                      markdown-mode
                      multi-term
                      multiple-cursors
                      nav
                      nix-mode
                      occidental-theme
                      omnisharp
                      password-store
                      pcmpl-args
                      php-mode
                      rainbow-mode
                      sauron
                      starter-kit
                      switch-window
                      textile-mode
                      undo-tree
                      workgroups
                      yaml-mode
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
