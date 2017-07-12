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
  (insert "#+FILETAGS: \n"))
