;; install packages for me
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not (member "elpa" (directory-files "~/.emacs.d")))
  (package-refresh-contents))
(defvar my-packages '(ac-ispell
                      matlab-mode
                      monokai-theme
                      multiple-cursors
                      org-table-comment))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; settings
(setq inhibit-splash-screen t)
(global-linum-mode t)
(setq linum-format "%3d ")
(setq auto-save-default nil)
(setq-default show-trailing-whitespace t)

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
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; mutliple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-p") 'mc/edit-lines)

;; spell checking
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

; plain text editing
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

