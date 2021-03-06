;; Main emacs configuration area

;;
;; AuxTeX
;;
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;
;; deft
;;
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

;;
;; dired / dired+
;;
(setq dired-dwim-target t) ;; Try to guess where to copy/move files (other
                           ;; window)
(setq diredp-hide-details-initially-flag nil) ;; Show details by default
(require 'dired+)

;;
;; erlang-mode
;;
(setq inferior-erlang-machine-options '("-sname" "emacs"))

;;
;; fci-mode
;;
(add-hook 'prog-mode-hook 'fci-mode t)

;;
;; go-mode
;;
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4 indent-tabs-mode t))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;
;; helm
;;
(setq helm-kill-ring-threshold 4)

;;
;; ibuffer-vc
;;
(require 'ibuffer-vc)
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (ibuffer-do-sort-by-alphabetic)))
;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))
;; Ibuffer configuration
(setq ibuffer-shrink-to-minimum-size t)
;; Always show last buffer even when it would be hidden
(setq ibuffer-always-show-last-buffer t)
;; Don't show empty groups
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-use-header-line t)
;; Set the cursor on the most recent buffer when switching to ibuffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;
;; ido
;;
(setq ido-show-dot-for-dired t)

;;
;; iedit, used for quick jumping & editing of symbols
;;
(require 'iedit)
(define-key iedit-lib-keymap (kbd "C-=") 'iedit-toggle-unmatched-lines-visible)

;;
;; key-chord
;;
(require 'key-chord)
(key-chord-mode 1)

;;
;; php-mode
;;
(defun my-php-mode-hook ()
  (setq indent-tabs-mode t)
  (let ((my-tab-width 4))
    (setq tab-width my-tab-width)
    (setq c-basic-indent my-tab-width)
    (set (make-local-variable 'tab-stop-list)
         (number-sequence my-tab-width 200 my-tab-width))))
(add-hook 'php-mode-hook 'my-php-mode-hook)

;;
;; switch-window
;;
(require 'switch-window)

;;
;; term settings
;;
;; Set ls output to something nicer
(setq dired-listing-switches "-alh --time-style=long-iso")
;; Don't save duplicates in the command history
(setq term-input-ignoredups t)
;; I want many lines of history in the buffer
(setq term-buffer-maximum-size 50000)
;; I want many lines of command history
(setq term-input-ring-size 5000)
;; Use better colors
(setq term-default-bg-color nil)
(setq term-default-fg-color "#DDDDDD")

;;
;; textile-mode
;;
(autoload 'textile-mode "textile-mode"
  "Textile editing mode." t)
(add-to-list 'auto-mode-alist
             '("\\.textile\\'". textile-mode))

;;
;; undo-tree
;;
(global-undo-tree-mode)

;;
;; whitespace-mode
;;
(setq whitespace-style '(face tabs trailing empty space-after-tab
                              space-before-tab tab-mark))

;;
;; workgroups
;;
(require 'workgroups)

;;
;; yaml-mode
;;
(add-hook 'yaml-mode-hook #'ansible-doc-mode)



;; UI settings

(scroll-bar-mode -1)
(setq display-time-24hr-format t)
;; Show status, percent charge and remaining time in battery-mode
(setq battery-mode-line-format "[%b%p%% %t] ")
(display-battery-mode t)
(display-time-mode t)

;; Show empty lines in fringe as standard
(setq-default indicate-empty-lines t)

;; Set position
(setq calendar-latitude 59.86)
(setq calendar-longitude 17.63)
(setq calendar-location-name "Uppsala")

;; Enable mouse in xterm
(xterm-mouse-mode)

;; Disable line highlighting of current line
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; Set color theme
(load-theme 'occidental t)



;; General edit settings
;; Set fill-column
(setq-default fill-column 80)
;; Show column numbers
(setq column-number-mode t)
;; Automatically revert files when they change
(global-auto-revert-mode 1)
;; Warn me if saving files with too long lines
(add-hook 'after-save-hook 'et-check-long-lines)
;; Always add newlines to eof
(setq-default require-final-newline t)


;; RefTeX settings
;; Show index in menu
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)



;; Backup file handling
(setq
 backup-by-copying t                          ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                           ; use versioned backups
;; Autosave file handling
(defvar my-auto-save-folder (concat "~/.emacs.d/auto-save"))
(setq auto-save-file-name-transforms `((".*", my-auto-save-folder t)))


;; Small handy functions
(defun make-shell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))



;; Key bindings

;; Replace standard buffer list with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Set global key for storing links in org mode
(global-set-key "\C-cl" 'org-store-link)

;; Set workgroup prefix key
(setq wg-prefix-key (kbd "C-c w"))

;; Set key for editing with multiple-cursors
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this-dwim)

;; Use switch-window instead of default function
(global-set-key (kbd "C-x o") 'switch-window)

;; Set a key for starting nav
(global-set-key [(f6)] 'nav)

;; Set key for magit invocation
(global-set-key [(f7)] 'magit-status)

;; Set key for starting dired
(global-set-key [(f8)] 'dired)

;; Set key for starting eshell and multi-term
(global-set-key [(f9)] 'make-shell)
(global-set-key (kbd "S-<f9>") 'multi-term)

;; Set key for helm-show-kill-ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Set additional key for helm-power
(global-set-key [(f11)] 'helm-power)

;; Set key chords
; frame actions
(key-chord-define-global "xo" 'other-window)
(key-chord-define-global "x1" 'delete-other-windows)
(key-chord-define-global "x0" 'delete-window)
(defun kill-this-buffer-if-not-modified ()
  (interactive)
  ; taken from menu-bar.el
  (if (menu-bar-non-minibuffer-window-p)
      (kill-buffer-if-not-modified (current-buffer))
    (abort-recursive-edit)))
(key-chord-define-global "xk" 'kill-this-buffer-if-not-modified)

; file actions
(key-chord-define-global "bf" 'ido-switch-buffer)
(key-chord-define-global "cf" 'ido-find-file)
(key-chord-define-global "zx" "\C-x\C-s")

;; Set keys for ace-jump-mode
(key-chord-define-global "az" 'ace-jump-mode)
;; TODO: Find good keybindings for these...
;;(key-chord-define-global "az" 'ace-jump-char-mode)
;;(key-chord-define-global "az" 'ace-jump-line-mode)



;; TODO: Add support for:
;; * distel
;; * color-theme-wombat+ (maybe not)

;;(add-to-list 'load-path "~/.emacs.d/vendor/edts")
;;(setq edts-erl-command "~/var/erlang-install/bin/erl")
;;(require 'edts-start)
