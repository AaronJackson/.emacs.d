;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE SOURCE SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not (member "elpa" (directory-files "~/.emacs.d")))
  (package-refresh-contents))
(defun check-installed (p)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold 33554432) ; 32MB before garbage collection
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(global-linum-mode t) ;; line numbers
(setq linum-format "%3d ")
(column-number-mode t)

(show-paren-mode t)
(setq show-paren-delay 0)

(setq auto-save-default t ;; auto save
      make-backup-files nil)

(setq indent-tabs-mode nil ;; tabs
      tab-width 4)

(setq-default show-trailing-whitespace t) ;; i hate it
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)

(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asj/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current"
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-/") 'asj/comment-or-uncomment-region-or-line)

(defun asj/disable-special ()
  (interactive)
  (setq show-trailing-whitespace nil)
  (linum-mode -1))





