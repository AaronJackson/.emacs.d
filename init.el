;; package listings to be installed
(defvar my-packages '(matlab-mode
                      php-mode
                      monokai-theme
                      js2-mode
                      ido-ubiquitous
                      deft
                      auctex
                      ))

;; install packages listed above
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
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
;; (setq-default show-trailing-whitespace t)
(setq-default tab-width 4)

;; GUI Stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'monokai t)

;; tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; window resizing
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-<down>") 'shrink-window)

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

;; spell checking
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; plain text editing
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; latex specific
(require 'tex)
(add-hook 'latex-mode-hook 'auto-fill-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(TeX-global-PDF-mode t)
(setq-default TeX-master nil)
(setq TeX-engine 'pdflatex)

;; deft
(setq deft-extension "txt")
(setq deft-directory "~/Documents/Deft")
(global-set-key [f8] 'deft)



;; mu4e
(load-file "~/.murc")


;; printing
(defun send-region-to-printer (&optional b e)
  (interactive)
  (shell-command-on-region
   b e
   "ssh orion.local \"iconv -f UTF-8 -t CP850 > /dev/lp0\""
   ))
    
