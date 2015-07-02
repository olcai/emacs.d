;;
;; my elisp utility functions
;;

(defun et-check-long-lines ()
  "Check if buffer has a line longer than `fill-column'."
  (interactive)
  (save-excursion
    (let ((len-found                  0)
          (found                      nil)
          (inhibit-field-text-motion  t))
      (goto-char (point-min)) ;; from the beginning
      (while (and (not found) (not (eobp)))
        (forward-line 1)
        (setq found (< fill-column
                       (setq len-found  (- (line-end-position) (point))))))
      (if found
          (message "Warning: line %d is %d chars long!" (line-number-at-pos)
                   len-found)))))

(defun et-pretty-print-xml-region (begin end)
  "Pretty print XML in region with xmllint."
  (interactive "r")
  (save-excursion
    (shell-command-on-region begin end "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region begin end nil)))

(defun et-pretty-print-xml-buffer ()
  "Pretty print XML in buffer with xmllint."
  (interactive)
  (et-pretty-print-xml-region (point-min) (point-max)))

(defun et-pretty-print-json-region (begin end)
  "Pretty print JSON in region with Python."
  (interactive "r")
  (save-excursion
    (shell-command-on-region begin end "python -m json.tool" (buffer-name) t)
    (js-mode)
    (indent-region begin end nil)))

(defun et-pretty-print-json-buffer ()
  "Pretty print JSON in buffer with Python."
  (interactive)
  (et-pretty-print-json-region (point-min) (point-max)))

(require 'tramp)
(defun et-sudired (dirname &optional switches)
  "Open directory in dired as root."
  (interactive (dired-read-dir-and-switches ""))
  (dired (concat "/sudo::/" (file-truename dirname)) switches))
