;;; auto-complete-clang-async-once.el --- Auto Completion source for clang for
;;; GNU Emacs

;; Copyright (C) 2010  Brian Jiang
;; Copyright (C) 2012  Taylan Ulrich Bayirli/Kammer

;; Authors: Brian Jiang <brianjcj@gmail.com>
;;          Golevka(?) [https://github.com/Golevka]
;;          Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;          Cjacker <cjacker@gmail.com>
;;          Many others
;; Keywords: completion, convenience
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Auto Completion source for clang.
;; Uses a "completion server" process to utilize libclang.
;; Also provides flymake syntax checking.

;; Cjacker: Made clang-complete server only run once for one emacs instance, and
;; change the source file internally

;;; Code:


(eval-when-compile
  (require 'cl))
(require 'auto-complete)


(defcustom ac-clang-complete-executable
  (executable-find "clang-complete")
  "Location of clang-complete executable."
  :group 'auto-complete
  :type 'file)

(defcustom ac-clang-lang-option-function nil
  "Function to return the lang type for option -x."
  :group 'auto-complete
  :type 'function)

(defcustom ac-clang-cflags nil
  "Extra flags to pass to the Clang executable.
This variable will typically contain include paths,
e.g., (\"-I~/MyProject\" \"-I.\")."
  :group 'auto-complete
  :type '(repeat (string :tag "Argument" "")))
(make-variable-buffer-local 'ac-clang-cflags)


(defconst ac-clang-completion-pattern
  "^COMPLETION: \\([^\s\n:]*\\)\\(?: : \\)*\\(.*$\\)")

(defun ac-clang-parse-output ()
  (goto-char (point-min))
  (let (lines
        match
        detailed-info
        (prev-match ""))
    (while (re-search-forward ac-clang-completion-pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (string= "Pattern" match)
        (setq detailed-info (match-string-no-properties 2))

        (if (string= match prev-match)
            (progn
              (when detailed-info
                (setq match
                      (propertize match
                                  'ac-clang-help (concat
                                                  (get-text-property
                                                   0 'ac-clang-help (car lines))
                                                  "\n"
                                                  detailed-info)))
                (setf (car lines) match)))
          (setq prev-match match)
          (when detailed-info
            (setq match (propertize match 'ac-clang-help detailed-info)))
          (push match lines))))
    lines))


(defconst ac-clang-error-buffer-name "*clang error*")


(defsubst ac-clang-create-position-string (pos)
  (save-excursion
    (goto-char pos)
    (format "row:%d\ncolumn:%d\n"
            (line-number-at-pos)
            (1+ (- (point) (line-beginning-position))))))

(defsubst ac-clang-lang-option ()
  ;; Determines the "-x" option to send to clang
  (or (and ac-clang-lang-option-function
           (funcall ac-clang-lang-option-function))
      (cond ((eq major-mode 'c++-mode)
             "c++")
            ((eq major-mode 'c-mode)
             "c")
            ((eq major-mode 'objc-mode)
             (cond ((string= "m" (file-name-extension (buffer-file-name)))
                    "objective-c")
                   (t
                    "objective-c++")))
            (t
             "c++"))))

(defsubst ac-clang-build-complete-args ()
  (append '("-cc1" "-fsyntax-only")
          (list "-x" (ac-clang-lang-option))
          ac-clang-cflags))


(defsubst ac-clang-clean-document (s)
  (when s
    (setq s (replace-regexp-in-string "<#\\|#>\\|\\[#" "" s))
    (setq s (replace-regexp-in-string "#\\]" " " s)))
  s)

(defun ac-clang-document (item)
  (if (stringp item)
      (let (s)
        (setq s (get-text-property 0 'ac-clang-help item))
        (ac-clang-clean-document s)))
  ;; (popup-item-property item 'ac-clang-help)
  )


(defface ac-clang-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for clang candidate"
  :group 'auto-complete)

(defface ac-clang-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the clang selected candidate."
  :group 'auto-complete)

(defsubst ac-clang-in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))


(defvar ac-clang-template-start-point nil)
(defvar ac-clang-template-candidates (list "ok" "no" "yes:)"))

(defun ac-clang-action ()
  (interactive)
  ;; (ac-last-quick-help)
  (let ((help (ac-clang-clean-document
               (get-text-property 0 'ac-clang-help (cdr ac-last-completion))))
        (raw-help (get-text-property 0 'ac-clang-help (cdr ac-last-completion)))
        (candidates (list)) ss fn args (ret-t "") ret-f)
    (setq ss (split-string raw-help "\n"))
    (dolist (s ss)
      (when (string-match "\\[#\\(.*\\)#\\]" s)
        (setq ret-t (match-string 1 s)))
      (setq s (replace-regexp-in-string "\\[#.*?#\\]" "" s))
      (cond ((string-match "^\\([^(<]*\\)\\(:.*\\)" s)
             (setq fn (match-string 1 s)
                   args (match-string 2 s))
             (push (propertize (ac-clang-clean-document args)
                               'ac-clang-help ret-t
                               'raw-args args)
                   candidates))
            ((string-match "^\\([^(]*\\)\\((.*)\\)" s)
             (setq fn (match-string 1 s)
                   args (match-string 2 s))
             (push (propertize (ac-clang-clean-document args)
                               'ac-clang-help ret-t
                               'raw-args args)
                   candidates)
             (when (string-match "\{#" args)
               (setq args (replace-regexp-in-string "\{#.*#\}" "" args))
               (push (propertize (ac-clang-clean-document args)
                                 'ac-clang-help ret-t
                                 'raw-args args)
                     candidates))
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize (ac-clang-clean-document args)
                                 'ac-clang-help ret-t
                                 'raw-args args)
                     candidates)))
            ;; check whether it is a function ptr
            ((string-match "^\\([^(]*\\)(\\*)\\((.*)\\)" ret-t)
             (setq ret-f (match-string 1 ret-t)
                   args (match-string 2 ret-t))
             (push (propertize args
                               'ac-clang-help ret-f
                               'raw-args "")
                   candidates)
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize args
                                 'ac-clang-help ret-f
                                 'raw-args "")
                     candidates)))))
    (cond (candidates
           (setq candidates (delete-dups candidates))
           (setq candidates (nreverse candidates))
           (setq ac-clang-template-candidates candidates)
           (setq ac-clang-template-start-point (point))
           (ac-complete-clang-template)

           (unless (cdr candidates) ;; unless length > 1
             (message (replace-regexp-in-string "\n" "   ;    " help))))
          (t
           (message (replace-regexp-in-string "\n" "   ;    " help))))))

(defun ac-clang-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (or (eq ?\. c)
                  ;; ->
                  (and (eq ?> c)
                       (eq ?- (char-before (1- (point)))))
                  ;; ::
                  (and (eq ?: c)
                       (eq ?: (char-before (1- (point))))))
          (point)))))

(defun ac-clang-same-count-in-string (c1 c2 s)
  (let ((count 0) (cur 0) (end (length s)) c)
    (while (< cur end)
      (setq c (aref s cur))
      (cond ((eq c1 c)
             (setq count (1+ count)))
            ((eq c2 c)
             (setq count (1- count))))
      (setq cur (1+ cur)))
    (= count 0)))

(defun ac-clang-split-args (s)
  (let ((sl (split-string s ", *")))
    (cond ((string-match "<\\|(" s)
           (let ((res (list)) (pre "") subs)
             (while sl
               (setq subs (pop sl))
               (unless (string= pre "")
                 (setq subs (concat pre ", " subs))
                 (setq pre ""))
               (cond ((and (ac-clang-same-count-in-string ?\< ?\> subs)
                           (ac-clang-same-count-in-string ?\( ?\) subs))
                      ;; (cond ((ac-clang-same-count-in-string ?\< ?\> subs)
                      (push subs res))
                     (t
                      (setq pre subs))))
             (nreverse res)))
          (t
           sl))))


(defun ac-clang-template-candidate ()
  ac-clang-template-candidates)

(defun ac-clang-template-action ()
  (interactive)
  (unless (null ac-clang-template-start-point)
    (let ((pos (point)) sl (snp "")
          (s (get-text-property 0 'raw-args (cdr ac-last-completion))))
      (cond ((string= s "")
             ;; function ptr call
             (setq s (cdr ac-last-completion))
             (setq s (replace-regexp-in-string "^(\\|)$" "" s))
             (setq sl (ac-clang-split-args s))
             (when (featurep 'yasnippet)
               (dolist (arg sl)
                 (setq snp (concat snp ", ${" arg "}")))
               (yas/expand-snippet (concat "("  (substring snp 2) ")")
                                   ac-clang-template-start-point pos)))
            (t
             (unless (string= s "()")
               (setq s (replace-regexp-in-string "{#" "" s))
               (setq s (replace-regexp-in-string "#}" "" s))
               (when (featurep 'yasnippet)
                 (setq s (replace-regexp-in-string "<#" "${" s))
                 (setq s (replace-regexp-in-string "#>" "}" s))
                 (setq s (replace-regexp-in-string ", \\.\\.\\." "}, ${..." s))
                 (yas/expand-snippet s ac-clang-template-start-point pos))))))))


(defun ac-clang-template-prefix ()
  ac-clang-template-start-point)


;; This source shall only be used internally.
(ac-define-source clang-template
  '((candidates . ac-clang-template-candidate)
    (prefix . ac-clang-template-prefix)
    (requires . 0)
    (action . ac-clang-template-action)
    (document . ac-clang-document)
    (cache)
    (symbol . "t")))


;;;
;;; Rest of the file is related to async.
;;;

(defvar ac-clang-status 'idle)
(defvar ac-clang-current-candidate nil)
(defvar ac-clang-completion-process nil)

(defvar current-clang-file "")

;;(make-variable-buffer-local 'ac-clang-status)
;;(make-variable-buffer-local 'ac-clang-current-candidate)
;;(make-variable-buffer-local 'ac-clang-completion-process)

;;;
;;; Functions to speak with the clang-complete process
;;;

(defun ac-clang-send-source-code (proc)
  (save-restriction
    (widen)
    (let ((buffer-string (buffer-substring-no-properties (point-min)
                                                         (point-max))))
      (process-send-string proc
                           (format "source_length:%d\n"
                                   (length (string-as-unibyte buffer-string))))
      (process-send-string proc buffer-string)
      (process-send-string proc "\n\n"))))

(defun ac-clang-send-reparse-request (proc)
  (save-restriction
    (widen)
    (process-send-string proc "SOURCEFILE\n")
    (ac-clang-send-source-code proc)
    (process-send-string proc "REPARSE\n\n")))

(defun ac-clang-send-completion-request (proc prefix)
  (if (not (string= current-clang-file (buffer-file-name)))
      (ac-clang-filechanged))
  (save-restriction
    (widen)
    (process-send-string proc (format "COMPLETION\n" prefix))
    (process-send-string proc (ac-clang-create-position-string (- (point) (length ac-prefix))))
    (process-send-string proc (format "prefix:%s\n" prefix))
    (ac-clang-send-source-code proc)))

(defun ac-clang-send-cmdline-args (proc)
  ;; send message head and num_args
  (process-send-string proc "CMDLINEARGS\n")
  (process-send-string proc (format "num_args:%d\n" (length (ac-clang-build-complete-args))))

  ;; send arguments
  (mapc (lambda (arg)
          (process-send-string proc (format "%s " arg)))
        (ac-clang-build-complete-args))
  
  (process-send-string proc "\n"))

(defun ac-clang-update-cmdlineargs ()
  (interactive)
  (ac-clang-send-cmdline-args ac-clang-completion-process))

(defun ac-clang-send-filechanged (proc)
  ;; send message head and num_args
  (process-send-string proc "FILECHANGED\n")
  (process-send-string
   proc (format "filename:%s\n" (buffer-file-name)))
  (process-send-string
   proc (format "num_args:%d\n" (length (ac-clang-build-complete-args))))
  ;; send arguments
  (mapc
   (lambda (arg)
     (process-send-string proc (format "%s " arg)))
   (ac-clang-build-complete-args))
  (process-send-string proc "\n"))

(defun ac-clang-filechanged ()
  (interactive)
  (if (not (string= current-clang-file (buffer-file-name)))
      (setq current-clang-file (buffer-file-name)))
  (ac-clang-send-filechanged ac-clang-completion-process))


(defun ac-clang-send-shutdown-command (proc)
  (process-send-string proc "SHUTDOWN\n"))


(defun ac-clang-append-process-output-to-process-buffer (process output)
  "Append process output to the process buffer."
  (with-current-buffer (process-buffer process)
    (save-excursion
      ;; Insert the text, advancing the process marker.
      (goto-char (process-mark process))
      (insert output)
      (set-marker (process-mark process) (point)))
    (goto-char (process-mark process))))

;;  Receive server responses (completion candidates) and fire auto-complete
(defun ac-clang-parse-completion-results (proc)
  (with-current-buffer (process-buffer proc)
    (ac-clang-parse-output)))

(defun ac-clang-filter-output (proc string)
  (ac-clang-append-process-output-to-process-buffer proc string)
  (when (string= (substring string -1 nil) "$")
    (setq ac-clang-current-candidate (ac-clang-parse-completion-results proc))
    (setq ac-clang-status 'acknowledged)
    (ac-start :force-init t)
    (ac-update)
    (setq ac-clang-status 'idle)))

(defun ac-clang-candidate ()
  (case ac-clang-status
    (idle
     (if (< (length ac-prefix) 1)
         (message "ac-clang-candidate triggered with short prefix")
       (message "ac-clang-candidate triggered - fetching candidates...")
       (setq ac-clang-saved-prefix ac-prefix)
       ;; NOTE: although auto-complete would filter the result for us, but when there's
       ;;       a HUGE number of candidates avaliable it would cause auto-complete to
       ;;       block. So we filter it uncompletely here, then let auto-complete filter
       ;;       the rest later, this would ease the feeling of being "stalled" at some degree.

       (message "saved prefix: %s" ac-clang-saved-prefix)
       (with-current-buffer (process-buffer ac-clang-completion-process)
         (erase-buffer))
       (setq ac-clang-status 'wait)
       (setq ac-clang-current-candidate nil)
       (ac-clang-send-completion-request ac-clang-completion-process ac-prefix)
       ac-clang-current-candidate))

    (wait
     (message "ac-clang-candidate triggered - wait")
     ac-clang-current-candidate)

    (acknowledged
     (message "ac-clang-candidate triggered - ack")
     (setq ac-clang-status 'idle)
     ac-clang-current-candidate)))


(defun ac-clang-shutdown-process ()
  (message "Shutting down clang process")
  (if ac-clang-completion-process
      (ac-clang-send-shutdown-command ac-clang-completion-process)))

(defun ac-clang-reparse-buffer ()
  (if ac-clang-completion-process
      (ac-clang-send-reparse-request ac-clang-completion-process)))

(defun ac-clang-launch-completion-process ()
  (interactive)
  (if ac-clang-completion-process
      (ac-clang-filechanged)
    (ac-clang-launch-completion-process-internal)))

(defun ac-clang-launch-completion-process-internal ()
  (interactive)
  (setq ac-clang-completion-process
        (let ((process-connection-type nil))
          (apply 'start-process
                 "clang-complete" "*clang-complete*"
                 ac-clang-complete-executable
                 (append (ac-clang-build-complete-args)
                         (list (buffer-file-name))))))

  (set-process-filter ac-clang-completion-process 'ac-clang-filter-output)
  (set-process-query-on-exit-flag ac-clang-completion-process nil)
  ;; Pre-parse source code.
  (ac-clang-send-reparse-request ac-clang-completion-process)

  ;; (add-hook 'kill-buffer-hook 'ac-clang-shutdown-process nil t)
  (add-hook 'kill-emacs-hook 'ac-clang-shutdown-process nil t)
  (add-hook 'before-save-hook 'ac-clang-reparse-buffer)

  ;; (add-hook 'before-save-hook 'ac-clang-reparse-buffer)
  
  ;; Pre-parse source code.
  (ac-clang-send-reparse-request ac-clang-completion-process))


(ac-define-source clang-async
  '((candidates . ac-clang-candidate)
    (candidate-face . ac-clang-candidate-face)
    (selection-face . ac-clang-selection-face)
    (prefix . ac-clang-prefix)
    (requires . 0)
    (document . ac-clang-document)
    (action . ac-clang-action)
    (cache)
    (symbol . "c")))


(provide 'auto-complete-clang-async)
;;; auto-complete-clang-async.el ends here
