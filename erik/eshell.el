;;
;; eshell settings
;;

;; Modify settings
(setq
 ;; Fix shell auto-complete
 eshell-prompt-regexp "^[^#$\n]*[#$] "
 ;; Don't fill the buffer completly
 eshell-buffer-maximum-lines 50000
 ;; Ignore duplicate commands
 eshell-hist-ignoredups t
 ;; Avoid cycle-completion
 eshell-cmpl-cycle-completions nil
 ;; Ignore SCM files when completing
 eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
 ;; Save shell history on exit
 eshell-save-history-on-exit t
 ;; Smart scrolling - jump to beginning of line
 eshell-where-to-jump 'begin
 ;; Smart scrolling - enable quick review
 eshell-review-quick-commands nil
 ;; Smart scrolling - save buffer history
 eshell-smart-space-goes-to-end t
 ;; Allow visual programs to use stuff such as <C-x>
 eshell-escape-control-x nil)

;; More settings after load
(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
               '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
     ;; Fix HOME key since em-rebind doesn't seem to work
     (add-hook 'eshell-mode-hook
               '(lambda () (define-key eshell-mode-map (kbd "<home>") 'eshell-bol)))

     ;; Add some programs to the list of programs that should be
     ;; executed in term-mode instead of eshell
     (setq eshell-visual-commands (append
                                   '("vim" "mc" "mcedit" "nano" "htop"
                                     "powertop" "ssh")
                                   eshell-visual-commands))
     ;; Add some stuff to completion
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

;; Manipulate path (shorten to ~ if in $HOME)
(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

;; Get current git branch name
(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string
                       (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]") 'face `(:foreground "dark green"))
      )))

;; Set fancy prompt containing current dir & git branch
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
            (if (> (length p-lst) 3)
                (concat
                 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                            (substring elm 0 1)))
                            (butlast p-lst 3)
                            "/")
                 "/"
                 (mapconcat (lambda (elm) elm)
                            (last p-lst 3)
                            "/"))
              (mapconcat (lambda (elm) elm)
                         p-lst
                         "/")))
          (split-string (pwd-repl-home (eshell/pwd)) "/"))
                     'face `(:foreground "brown"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (if (= (user-uid) 0)
             (propertize "# " 'face 'default)
           (propertize "$ " 'face 'default)))))

;; Don't highlight - needed to make colors get through
(setq eshell-highlight-prompt nil)

;; Define own commands
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

(defun eshell/cdl ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "lib")))

(defun eshell/cdg ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

(when (not (functionp 'eshell/rgrep))
  (defun eshell/rgrep (&rest args)
    "Use Emacs grep facility instead of calling external grep."
    (eshell-grep "rgrep" args t)))

(defun eshell/extract (file)
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not extract the file:'")))))
    (eshell-command-result (concat command " " file))))
