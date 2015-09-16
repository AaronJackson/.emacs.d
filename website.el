(defun asj/new-blog-post (n)
  (interactive "sFilename: ")
  (find-file (concat
              "~/public_html/blog/"
              (format-time-string "%Y-%m-%d-" (current-time))
              n ".org"))
  (insert "#+TITLE: \n")
  (insert "#+DATE: ")
  (org-insert-time-stamp (current-time) t)
  (insert "\n")
  (insert "#+FILETAGS: \n")
  (insert "#+INCLUDE: includes/after-title.org\n"))

(defun asj/website-make ()
  (interactive)
  (shell-command "~/public_html/scripts/generate_tags.sh")
  (org-publish-project "website"))

(defun asj/website-publish ()
  (interactive)
  (asj/website-make)
  (shell-command "~/public_html/scripts/publish.sh"))

(setq org-publish-project-alist
      '(("website"
         :components ("website-posts"
		      "website-tags"
		      "website-pages"))
        ("website-posts"
         :base-directory "~/public_html/blog/"
         :base-extension "org"
         :publishing-directory "~/public_html/blog/"
         :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head "
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"/style.css\" />"
         :html-postamble "
<p><a href=\"./index.html\">Blog Index</a>
&bull; <a href=\"./tags/index.html\">Posts by Tag</a>
&bull; <a href=\"../index.html\">Home</a></p>"
         :html-preamble "
<p><a href=\"./index.html\">Blog Index</a>
&bull; <a href=\"./tags/index.html\">Posts by Tag</a>
&bull; <a href=\"../index.html\">Home</a></p>"
         :html-inline-images t
         :with-toc nil
         :headline-numbering nil
         :section-number nil
     	 :auto-sitemap t
         :sitemap-filename "index.org"
         ;; :sitemap-function blog-sitemap-function
         :sitemap-title "Aaron S. Jackson - Blog"
         :sitemap-sort-files anti-chronologically
         :sitemap-date-format "%d %B %Y %H:%M"
         :sitemap-file-entry-format "%d - %t")
        ("website-tags"
         :base-directory "~/public_html/blog/tags/"
         :base-extension "org"
         :publishing-directory "~/public_html/blog/tags/"
         :publishing-function  org-html-publish-to-html
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head "
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"/style.css\" />"
         :html-postamble "
<p><a href=\"index.html\">All Tags</a>
&bull; <a href=\"../index.html\">Blog Index</a></p>"
         :html-preamble "
<p><a href=\"index.html\">All Tags</a>
&bull; <a href=\"../index.html\">Blog Index</a></p>")
        ("website-pages"
         :base-directory "~/public_html/"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head "
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"/style.css\" />"
         :html-postamble "
<p><a href=\"index.html\">&larr; Return Home</a></p>"
         :html-inline-images t
         :with-toc nil
         :headline-numbering nil
         :section-number nil)))
