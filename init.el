;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE SOURCE SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not (member "elpa" (directory-files "~/.emacs.d")))
  (package-refresh-contents))
(defun check-installed (p)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)

(global-linum-mode t) ;; line numbers
(setq linum-format "%3d ")

(setq auto-save-default nil ;; auto save
      make-backup-files nil)

(setq indent-tabs-mode nil ;; tabs
      tab-width 4)

(setq-default show-trailing-whitespace t) ;; i hate it
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)

(tool-bar-mode -1) ;; gui / style
(menu-bar-mode -1)
;; (check-installed 'ample-theme)
;; (load-theme 'ample-light t)
(check-installed 'monokai-theme)
(load-theme 'monokai t)

(add-hook 'image-mode-hook 'asj/disable-special)
(add-hook 'doc-view-mode-hook 'asj/disable-special)
(setq doc-view-resolution 500) ;; default is too low


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
  (setq show-trailing-whitespace nil)
  (linum-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE / CALENDARING / BLOGGING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (octave . t)
   (python . t)))
(setq org-src-fontify-natively t)
(setq org-agenda-files
      (list "/home/aaron/Documents/Organisation/Agenda/"))

;; calendar
(check-installed 'calfw)
(require 'calfw-org)
(add-hook 'cfw:calendar-mode-hook 'asj/disable-special)
(global-set-key [f7] 'cfw:open-org-calendar)

(defun asj/new-blog-post (n)
  (interactive "sFilename: ")
  (find-file (concat
	      "~/public_html/blog/org/"
	      (format-time-string "%Y-%m-%d-" (current-time))
	      n ".org"))
  (insert "#+TITLE: \n")
  (insert "#+DATE: ")
  (org-insert-time-stamp (current-time) t)
  (insert "\n")
  (insert "#+FILETAGS: \n")
  (insert "#+INCLUDE: ../incl-after-title.org\n"))

(defun asj/org-html-publish-to-html-with-tags (plist filename pubdir)
  (shell-command "~/public_html/scripts/generate_tags.sh")
  (org-html-publish-to-html plist filename pubdir))

(setq org-publish-project-alist
      '(("posts"
         :base-directory "~/public_html/blog/org/"
         :base-extension "org"
         :publishing-directory "~/public_html/blog/"
         :publishing-function asj/org-html-publish-to-html-with-tags
         :html-head-include-default-style nil
         :html-head-include-scripts nil
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\" />"
         :html-postamble "<p><a href=\"./index.html\">Blog Index</a>
			  &bull; <a href=\"./tags/index.html\">Posts by Tag</a>
			  &bull; <a href=\"../index.html\">Home</a></p>"
	 :html-inline-images t
         :with-toc nil
         :headline-numbering nil
         :section-number nil
     	 :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Aaron S. Jackson - Blog"
         :sitemap-sort-files anti-chronologically
         :sitemap-date-format "%e %B %Y %H:%M"
         :sitemap-file-entry-format "%d - %t")
	("tags"
	 :base-directory "~/public_html/blog/tags/"
	 :base-extension "org"
	 :publishing-directory "~/public_html/blog/tags/"
	 :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil
         :html-head-include-scripts nil
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../../style.css\" />"
         :html-postamble "<p><a href=\"index.html\">All Tags</a>
                          &bull; <a href=\"../index.html\">Blog Index</a></p>")
	("pages"
         :base-directory "~/public_html/"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil
         :html-head-include-scripts nil
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
	 :html-postamble "<p><a href=\"index.html\">&larr; Return Home</a></p>"
	 :html-inline-images t
         :with-toc nil
         :headline-numbering nil
         :section-number nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-installed 'ido-ubiquitous)
(ido-mode t)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPELL CHECKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLAIN TEXT EDITING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LATEX EDITING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-installed 'auctex)
(require 'tex)
(add-hook 'latex-mode-hook 'auto-fill-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(TeX-global-PDF-mode t)
(setq-default TeX-master nil)
(setq TeX-engine 'pdflatex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-installed 'deft)
(setq deft-extension "txt")
(setq deft-directory "~/Documents/Organisation/Deft")
(global-set-key [f8] 'deft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATLAB STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-installed 'matlab-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MU4E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/mu4e-config.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WEB DEVELOPMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-installed 'web-mode)


(add-hook 'web-mode-hook
	  '(lambda ()
	     (setq web-mode-markup-indent-offset 2
		   web-mode-css-indent-offset 2
		   web-mode-code-indent-offset 2)
	     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTO COMPLETION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-installed 'company)
(setq company-idle-delay 0)
(global-company-mode)

(check-installed 'company-math)
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (add-to-list 'company-backends 'company-math-symbols-latex)
	    (add-to-list 'company-backends 'company-latex-commands)))
