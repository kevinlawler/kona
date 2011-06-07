;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (c) 2011 Scott Vokes <vokes.s@gmail.com>
;; 
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; major-mode and a few utilities for working with kona (k).
;; Usage:
;;
;; (require 'k-mode)
;;
;; Bind switch-to-k to something convenient, e.g.
;;   (global-set-key (kbd "C-c i k") 'switch-to-k)
;; and use that to start a connected k session.
;;
;; Use k-send (C-c C-e) to send the region (if any) or current line
;; or k-send-buffer (C-c C-b) to send blocks of code to it.
;;
;; TODO
;; * smart indentation
;; * syntax-table
;; * custom stuff? (I don't use it...)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'comint)

(defvar k-program-name "k" "k executable name.")

(defvar k-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-e") 'k-send)    ;eval region or line
    (define-key m (kbd "C-c C-z") 'switch-to-k)
    (define-key m (kbd "C-c C-b") 'k-send-buffer)
    ;(define-key m (kbd "C-c C-l") 'k-load-file)
    ;; (define-key m (kbd ")") 'k-electric-rparen)
    ;; (define-key m (kbd "]") 'k-electric-rbrace)
    ;; (define-key m (kbd "}") 'k-electric-rcurly)
    ;; (define-key m (kbd "\"") 'k-electric-quote)
    m)
  "Keymap for k mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface k-normal-face
    '((t (:inherit nil)))
  "Font lock for text with no special highlighting.")

(defface k-builtin-face
    '((t (:inherit font-lock-builtin-face)))
  "Font lock for builtins, such as _draw.")

(defface k-number-face
    '((t (:inherit k-normal-face)))
  "Font lock for numbers.")

(defface k-variable-face
    '((t (:inherit k-normal-face)))
  "Font lock for variables.")

(defface k-variable-binding-face
    '((t (:inherit font-lock-variable-name-face)))
  "Font lock for variable bindings sites.")

(defface k-verb-face
    '((t (:inherit k-normal-face)))
  "Font lock for verbs.")

(defface k-string-face
    '((t (:inherit font-lock-string-face)))
  "Font lock for strings.")

(defface k-symbol-face
    '((t (:inherit font-lock-constant-face)))
  "Font lock for symbols.")

(defface k-adverb-face
    '((t (:weight bold :inherit font-lock-keyword-face)))
  "Font lock for adverbs.")

(defface k-comment-delimeter-face
    '((t (:inherit font-lock-comment-delimeter-face)))
  "Font lock for comment marker.")

(defface k-comment-face
    '((t (:inherit font-lock-comment-face)))
  "Font lock for comments.")

(defface k-brace-face
    '((t (:inherit font-lock-function-name-face)))
  "Font lock for {}s.")

(defface k-bracket-face
    '((t (:weight bold :inherit font-lock-normal-face)))
  "Font lock for []s.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME:
;; * vars_with_underscores are highlighted wrong.
;; * other corner cases?

(defun k-font-lock-keyword-maker ()
  '(("^\\(/\\) \\([^\n]*\\)$" 
     (1 'k-comment-delimeter-face)
     (2 'k-comment-face))
    (" \\(/\\) \\([^\n]*\\)$" 
     (1 'k-comment-delimeter-face)
     (2 'k-comment-face))
    ("[/\\']:?" . 'k-adverb-face)
    ("`\"[^\"]*\"" . 'k-symbol-face)
    ("\"[^\"]*\"" . 'k-string-face)
    (";" . 'font-lock-keyword-face)
    ("_[a-zA-Z]+" . 'k-builtin-face)
    ("[a-zA-Z][a-zA-Z0-9]*:" . 'k-variable-binding-face)
    ("[a-zA-Z][a-zA-Z0-9]*" . 'k-variable-face)
    ("-?[0-9]+\\(\\.?[0-9]*\\)?\\([eE][+-]?[0-9]+\\)?" . 'k-number-face)
    ("\\(`\\)\\([a-zA-Z][a-zA-Z0-9_]*\\)" 
     (1 'k-builtin-face)
     (2 'k-symbol-face))
    ("[!#$%&*+,.;<=>?@^_|~-:]:?" . 'k-verb-face)
    ("[{}]" . 'k-brace-face)
    ("[][]" . 'k-bracket-face)))
; (setq k-font-lock-keywords (k-font-lock-keyword-maker))

(defvar k-font-lock-keywords
  (k-font-lock-keyword-maker)
   "Keyword highlighting specification for `k-mode'.")

(defvar k-mode-hook nil "Hooks called when starting k-mode.")

;; (defvar k-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     TODO
;;     st))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode k-mode nil "k"
  "Major mode for k code.

\\{k-mode-map}
"
  ; :syntax-table k-mode-syntax-table
  (set (make-local-variable 'comment-start) "/ ")
  (set (make-local-variable 'comment-end) "")
  (use-local-map k-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(k-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.k$" . k-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun k-proc ()
  "Get k process."
  (get-process k-program-name))

(defun k-proc-buffer ()
  "Get k process's buffer."
  (get-buffer (concat "*" k-program-name "*")))

(defun switch-to-k (uarg)
  "Switch to a k process, or spawn a new one if not running.
Universal argument switches to it in another window."
  (interactive "P")
  (let* ((kbuf (k-proc-buffer))
         (kproc (k-proc)))
    (unless (and kbuf kproc)
      (setq kbuf (make-comint k-program-name k-program-name))
      (with-current-buffer kbuf
        (add-hook 'comint-output-filter-functions
                  'k-comint-output-filter nil t)))
    (unless (equal (current-buffer) kbuf)
      (if uarg
          (switch-to-buffer-other-window kbuf)
          (switch-to-buffer kbuf)))))

(defun k-send-str (s)
  "Send string to the k process, if existing."
  ;; TODO: print comint result in minibuf?
  (let ((kproc (k-proc))
        (kbuf (k-proc-buffer)))
    (when (and kproc s)
      (comint-send-string kproc s))))
            
(defun k-comint-output-filter (s)
  "Print output from code sent to k in the minibuffer."
  (princ (substring s 0 -3)))  ; -3 for \n(space space)

(defun k-send-region (start end)
  "Send region to k process."
  (interactive "r")
  (let ((str (concat (buffer-substring start end) "\n")))
    (k-send-str str)))

(defun k-send-buffer ()
  "Send whole buffer to k process."
  (interactive)
  (k-send-region (point-min) (point-max)))

(defun k-send-line ()
  "Send current line to k process."
  (interactive)
  (save-excursion
    (let ((bol (progn (beginning-of-line) (point)))
          (eol (progn (end-of-line) (point))))
      (let ((str (concat (buffer-substring bol eol) "\n")))
        (k-send-str str)))))

(defun k-send ()
  "Send current line or region to k process."
  (interactive)
  (if mark-active
      (k-send-region (region-beginning) (region-end))
      (k-send-line)))

(provide 'k-mode)
