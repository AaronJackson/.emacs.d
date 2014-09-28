;; package listings to be installed
(defvar my-packages '(ac-ispell
                      matlab-mode
                      php-mode
                      php-refactor-mode
                      monokai-theme
                      multiple-cursors
                      org-table-comment
                      js2-mode
                      clojure-mode
                      cider
                      ac-math
                      ac-js2
                      ac-nrepl
                      ac-python
                      ido-ubiquitous))

;; install packages listed above
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not (member "elpa" (directory-files "~/.emacs.d")))
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; settings
(setq inhibit-splash-screen t)
(global-linum-mode t)
(setq linum-format "%3d ")
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq-default show-trailing-whitespace t)
(setq-default tab-width 4)

;; GUI Stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'monokai t)

;; tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Generic clicking
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; ido mode
(require 'ido)
(ido-mode t)

;; matlab stuff
(require 'matlab)
(setq exec-path (append exec-path '("/usr/local/MATLAB/R2013a_Student/bin")))
(add-hook 'matlab-mode-hook
          (lambda ()
            (define-key matlab-mode-map (kbd "<f5>") 'matlab-shell-save-and-go)
            (define-key matlab-mode-map (kbd "<f6>") 'matlab-shell-run-cell)
            )
          )

;; general shortcuts
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)

;; commenting code
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; mutliple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-?") 'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; spell checking
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; plain text editing
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; latex specific
(add-hook 'latex-mode-hook 'auto-fill-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(setq exec-path (append exec-path '("/usr/texbin")))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-engine 'pdflatex)

;; email
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-trash-folder  "/Trash")
