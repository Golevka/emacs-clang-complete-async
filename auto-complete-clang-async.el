(provide 'auto-complete-clang-async)
(require 'auto-complete)


(defvar ac-clang-status   'idle)
(defvar current-candidate  nil)
(defvar completion-proc    nil)
(defvar ac-clang-saved-prefix "")

(make-variable-buffer-local 'ac-clang-status)
(make-variable-buffer-local 'current-candidate)
(make-variable-buffer-local 'completion-proc)



;;/===========================================================================\
;;       Brian J's original auto-clang-complete code, do not touch.
;;

;;; auto-complete-clang.el --- Auto Completion source for clang for GNU Emacs

;; Copyright (C) 2010  Brian Jiang

;; Author: Brian Jiang <brianjcj@gmail.com>
;; Keywords: completion, convenience
;; Version: 0.1e

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
;;
;; Auto Completion source for clang. Most of codes are taken from
;; company-clang.el and modified and enhanced for Auto Completion.

;;; Code:


(defcustom clang-complete-executable
  (executable-find "clang-complete")
  "*Location of clang-complete executable"
  :group 'auto-complete
  :type 'file)


(defcustom ac-clang-lang-option-function nil
  "*function to return the lang type for option -x."
  :group 'auto-complete
  :type 'function)

;;; Extra compilation flags to pass to clang.
(defcustom ac-clang-flags nil
  "Extra flags to pass to the Clang executable.
This variable will typically contain include paths, 
e.g., ( \"-I~/MyProject\", \"-I.\" )."
  :group 'auto-complete
  :type '(repeat (string :tag "Argument" "")))

;;; The prefix header to use with Clang code completion. 
(defvar ac-clang-prefix-header nil)


;;; Set the Clang prefix header
(defun ac-clang-set-prefix-header (ph)
  (interactive
   (let ((def (car (directory-files "." t "\\([^.]h\\|[^h]\\).pch\\'" t))))
     (list
      (read-file-name (concat "Clang prefix header(current: " ac-clang-prefix-header ") : ")
                      (when def (file-name-directory def))
                      def nil (when def (file-name-nondirectory def))))))
  (cond ((string-match "^[\s\t]*$" ph)
         (setq ac-clang-prefix-header nil))
        (t
         (setq ac-clang-prefix-header ph))))

;;; Set a new cflags for clang
(defun ac-clang-set-cflags ()
  "set new cflags for clang from input string"
  (interactive)
  (setq ac-clang-flags (split-string (read-string "New cflags: ")))
  (ac-clang-update-cmdlineargs))

;;; Set new cflags from shell command output
(defun ac-clang-set-cflags-from-shell-command ()
  "set new cflags for ac-clang from shell command output"
  (interactive)
  (setq ac-clang-flags
    (split-string
     (shell-command-to-string
      (read-shell-command "Shell command: " nil nil
                          (and buffer-file-name
                               (file-relative-name buffer-file-name))))))
  (ac-clang-update-cmdlineargs))


(defconst ac-clang-completion-pattern
  "^COMPLETION: \\(%s[^\s\n:]*\\)\\(?: : \\)*\\(.*$\\)")


(defun ac-clang-parse-output (prefix)
  (goto-char (point-min))
  (let ((pattern (format ac-clang-completion-pattern
                         (regexp-quote prefix)))
        lines match detailed_info
        (prev-match ""))
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (string= "Pattern" match)
        (setq detailed_info (match-string-no-properties 2))
      
        (if (string= match prev-match)
            (progn
              (when detailed_info
                (setq match (propertize match
                                        'ac-clang-help
                                        (concat
                                         (get-text-property 0 'ac-clang-help (car lines))
                                         "\n"
                                         detailed_info)))
                (setf (car lines) match)
                ))
          (setq prev-match match)
          (when detailed_info
            (setq match (propertize match 'ac-clang-help detailed_info)))
          (push match lines))))
    lines))


(defsubst ac-clang-build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "row:%d\ncolumn:%d\n" 
            (line-number-at-pos)
            (1+ (- (point) (line-beginning-position))))))

(defsubst ac-clang-lang-option ()
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

(defsubst ac-clang-build-args ()
  (append (list "-x" (ac-clang-lang-option))
          ac-clang-flags
          (when (stringp ac-clang-prefix-header)
            (list "-include-pch" (expand-file-name ac-clang-prefix-header)))))


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



(defvar ac-template-start-point nil)
(defvar ac-template-candidates (list "ok" "no" "yes:)"))

(defun ac-clang-action ()
  (interactive)
  ;; (ac-last-quick-help)
  (let ((help (ac-clang-clean-document (get-text-property 0 'ac-clang-help (cdr ac-last-completion))))
        (raw-help (get-text-property 0 'ac-clang-help (cdr ac-last-completion)))
        (candidates (list)) ss fn args (ret-t "") ret-f)
    (setq ss (split-string raw-help "\n"))
    (dolist (s ss)
      (when (string-match "\\[#\\(.*\\)#\\]" s)
        (setq ret-t (match-string 1 s)))
      (setq s (replace-regexp-in-string "\\[#.*?#\\]" "" s))
      (cond ((string-match "^\\([^(]*\\)\\((.*)\\)" s)
             (setq fn (match-string 1 s)
                   args (match-string 2 s))
             (push (propertize (ac-clang-clean-document args) 'ac-clang-help ret-t
                               'raw-args args) candidates)
             (when (string-match "\{#" args)
               (setq args (replace-regexp-in-string "\{#.*#\}" "" args))
               (push (propertize (ac-clang-clean-document args) 'ac-clang-help ret-t
                                 'raw-args args) candidates))
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize (ac-clang-clean-document args) 'ac-clang-help ret-t
                                 'raw-args args) candidates)))
            ((string-match "^\\([^(]*\\)(\\*)\\((.*)\\)" ret-t) ;; check whether it is a function ptr
             (setq ret-f (match-string 1 ret-t)
                   args (match-string 2 ret-t))
             (push (propertize args 'ac-clang-help ret-f 'raw-args "") candidates)
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize args 'ac-clang-help ret-f 'raw-args "") candidates)))))
    (cond (candidates
           (setq candidates (delete-dups candidates))
           (setq candidates (nreverse candidates))
           (setq ac-template-candidates candidates)
           (setq ac-template-start-point (point))
           (ac-complete-template)
           
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


(defun ac-template-candidate ()
  ac-template-candidates)

(defun ac-template-action ()
  (interactive)
  (unless (null ac-template-start-point)
    (let ((pos (point)) sl (snp "")
          (s (get-text-property 0 'raw-args (cdr ac-last-completion))))
      (cond ((string= s "")
             ;; function ptr call
             (setq s (cdr ac-last-completion))
             (setq s (replace-regexp-in-string "^(\\|)$" "" s))
             (setq sl (ac-clang-split-args s))
             (cond ((featurep 'yasnippet)
                    (dolist (arg sl)
                      (setq snp (concat snp ", ${" arg "}")))
                    (condition-case nil
                        (yas/expand-snippet (concat "("  (substring snp 2) ")")
                                            ac-template-start-point pos) ;; 0.6.1c
                      (error
                       ;; try this one:
                       (ignore-errors (yas/expand-snippet
                                       ac-template-start-point pos
                                       (concat "("  (substring snp 2) ")"))) ;; work in 0.5.7
                       )))
                   ((featurep 'snippet)
                    (delete-region ac-template-start-point pos)
                    (dolist (arg sl)
                      (setq snp (concat snp ", $${" arg "}")))
                    (snippet-insert (concat "("  (substring snp 2) ")")))
                   (t
                    (message "Dude! You are too out! Please install a yasnippet or a snippet script:)"))))
             (t
             (unless (string= s "()")
               (setq s (replace-regexp-in-string "{#" "" s))
               (setq s (replace-regexp-in-string "#}" "" s))
               (cond ((featurep 'yasnippet)
                      (setq s (replace-regexp-in-string "<#" "${" s))
                      (setq s (replace-regexp-in-string "#>" "}" s))
                      (setq s (replace-regexp-in-string ", \\.\\.\\." "}, ${..." s))
                      (condition-case nil
                          (yas/expand-snippet s ac-template-start-point pos) ;; 0.6.1c
                        (error
                         ;; try this one:
                         (ignore-errors (yas/expand-snippet ac-template-start-point pos s)) ;; work in 0.5.7
                         )))
                     ((featurep 'snippet)
                      (delete-region ac-template-start-point pos)
                      (setq s (replace-regexp-in-string "<#" "$${" s))
                      (setq s (replace-regexp-in-string "#>" "}" s))
                      (setq s (replace-regexp-in-string ", \\.\\.\\." "}, $${..." s))
                      (snippet-insert s))
                     (t
                      (message "Dude! You are too out! Please install a yasnippet or a snippet script:)")))))))))


(defun ac-template-prefix ()
  ac-template-start-point)


;; this source shall only be used internally.
(ac-define-source template
  '((candidates . ac-template-candidate)
    (prefix . ac-template-prefix)
    (requires . 0)
    (action . ac-template-action)
    (document . ac-clang-document)
    (cache)
    (symbol . "t")))






;;/============================================================================\
;;         The most "primitive" message transmition functions
;;
(defun send-source-code (proc)  
    (process-send-string 
     proc (format "source_length:%d\n" 
                  (length (string-as-unibyte   ; fix non-ascii character problem
                           (buffer-substring-no-properties (point-min) (point-max)))
                          )))
    (process-send-string 
     proc (buffer-substring-no-properties (point-min) (point-max)))
    (process-send-string proc "\n\n")) ; bullet proof :-)

(defun send-reparse-request (proc)
  (save-restriction
    (widen)
    (process-send-string proc "SOURCEFILE\n")
    (send-source-code proc)  ; send source code first
    (process-send-string proc "REPARSE\n\n")) ; and then the reparse command
  )

(defun send-completion-request (proc)
  (save-restriction
    (widen)
    ;; send message header
    (process-send-string proc "COMPLETION\n")
    (process-send-string 
     proc (ac-clang-build-location (- (point) (length ac-prefix))))
    ;; send source code
    (send-source-code proc)))

(defun send-cmdline-args (proc)
  ;; send message head and num_args
  (process-send-string proc "CMDLINEARGS\n")
  (process-send-string 
   proc (format "num_args:%d\n" (length (ac-clang-build-args))))

  ;; send arguments
  (mapc 
   (lambda (arg) 
     (process-send-string proc (format "%s " arg)))
   (ac-clang-build-args))
  (process-send-string proc "\n") ; bullet proof
)

(defun ac-clang-update-cmdlineargs ()
  (interactive)
  (send-cmdline-args completion-proc))

(defun send-shutdown-command (proc)
  (process-send-string proc "SHUTDOWN\n"))


;;/===============================================================================\
;;  Recieve server responses (completion candidates) and fire auto-complete
;;
(defun parse-completion-results (proc)
  (with-current-buffer (process-buffer proc)
    (ac-clang-parse-output ac-clang-saved-prefix)))

(defun filter-output (proc string)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      ;; Insert the text, advancing the process marker.
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point)))
    (goto-char (process-mark proc)))
  
  (if (string= (substring string -1 nil) "$")
      (cond ((not (eq ac-clang-status 'preempted))
             (setq current-candidate (parse-completion-results proc))
             ;; (message "ac-clang results arrived")
             (setq ac-clang-status 'ack)
             (ac-start :force-init t) (ac-update)  ; it is critical to complete members, so 
                                                   ; we force ac-start to show up.
             (setq ac-clang-status 'idle))

            ((eq ac-clang-status 'preempted)
             (setq ac-clang-status 'idle)
             (ac-start)(ac-update)))
))


(defun ac-clang-candidate ()
  (cond ((eq ac-clang-status 'idle)
         ;; (message "ac-clang-candidate triggered - fetching candidates...")
         (setq ac-clang-saved-prefix ac-prefix) ; save completion prefix, 
                                                ; it would later be used by result filter

         ;; NOTE: although auto-complete would filter the result for us, but when there's
         ;;       a HUGE number of candidates avaliable it would cause auto-complete to 
         ;;       block. So we filter it uncompletely here, then let auto-complete filter
         ;;       the rest later, this would ease the feeling of being "stalled" at some degree.

         ;; (message "saved prefix: %s" ac-clang-saved-prefix)
         (with-current-buffer 
             (process-buffer completion-proc) (erase-buffer))
         (setq ac-clang-status 'wait)
         (setq current-candidate nil)

         ;; send completion request
         (send-completion-request completion-proc)
         current-candidate)

        ((eq ac-clang-status 'wait) ; patient, we are waiting for the candidates...
         ;; (message "ac-clang-candidate triggered - wait")
         current-candidate)

        ((eq ac-clang-status 'ack) ; acknowledged...
         ;; (message "ac-clang-candidate triggered - ack")
         (setq ac-clang-status 'idle)
         current-candidate)

        ((eq ac-clang-status 'preempted)
         ;; (message "clang-async is preempted by a critical request")
         nil)
  ))


;; launch completion process
(defun launch-completion-proc ()
  ;; launch the process
  (setq completion-proc 
        (let ((process-connection-type nil)) 
          (apply 'start-process 
                 "clang-complete" "*clang-complete*" 
                 clang-complete-executable
                 (append (ac-clang-build-args) 
                         (list (buffer-file-name))))))

  ;; hook filter to it
  (set-process-filter completion-proc 'filter-output)
  ;; and let it preparse the source code...
  (send-reparse-request completion-proc)

  ;; hooks to make the clang-complete server shutdown when the buffer is killed. 
  ;; and reparse the source file automatically when the buffer is saved.
  ;; this hook is buffer local
  (add-hook 'kill-buffer-hook 
            (lambda () 
              (if completion-proc 
                  (send-shutdown-command completion-proc))) nil t)
  (add-hook 'before-save-hook
            (lambda ()
              (if completion-proc 
                  (send-reparse-request completion-proc))))

  (local-set-key (kbd ".") 'ac-clang-async-preemptive)
  (local-set-key (kbd ":") 'ac-clang-async-preemptive)
  (local-set-key (kbd ">") 'ac-clang-async-preemptive)
)


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




;;/===============================================================================\
;;     Handle . -> :: preemptively

(defun ac-clang-async-preemptive ()
  (interactive)
  (self-insert-command 1)
  (if (eq ac-clang-status 'idle)
      (ac-start)    ; clang-complete is currently idle, just emit it normally
    (setq ac-clang-status 'preempted)   ; it is working with the previous
                                        ; completion request, inform the proc
                                        ; filter to discard the previous result
                                        ; and emit another request.
))
