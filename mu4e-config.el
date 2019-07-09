;; NOTES: To protect from spam, I have rot13 the email addresses in
;;        this file. The script will decode these though.  The script
;;        allows for multiple mail accounts to operate by calling
;;        mail/pls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)
(setq mu4e-mu-binary "/usr/local/bin/mu"
      mu4e-get-mail-command "true"
      mu4e-update-interval 3600
     mu4e-html2text-command 'mu4e-shr2text
      mu4e-msg2pdf "~/usr/bin/msg2pdf" ;; requires maildir-utils-extra
      mu4e-maildir "~/Maildir"
      message-kill-buffer-on-exit t
      mu4e-sent-messages-behavior 'sent)

(defun mail/mu4e-default-hook ()
  (setq show-trailing-whitespace nil)
  (linum-mode -1))

;; this is needed if using mbsync instead of offlineimap
(setq mu4e-change-filenames-when-moving t)

(add-hook 'mu4e-headers-mode-hook 'mail/mu4e-default-hook)
(add-hook 'mu4e-compose-mode-hook 'mail/mu4e-default-hook)
(add-hook 'mu4e-view-mode-hook    'mail/mu4e-default-hook)

(setq mu4e-index-cleanup t
      mu4e-index-lazy-check nil
      mu4e-headers-include-related nil)

(setq mu4e-compose-signature-auto-include t
      mail-signature-file "~/.signature")

(setq mu4e-compose-complete-only-after
      (format-time-string
       "%Y-%m-%d" ;; I only want past six months
       (time-subtract (current-time)
		      (seconds-to-time 15552000))))

(setq mu4e-headers-fields
      '((:date          .  10)
	(:flags         .   6)
	(:from-or-to    .  22)
	(:mailing-list  .  10)
	(:subject       .  nil))
      mu4e-headers-from-or-to-prefix nil)

(add-to-list 'mu4e-view-actions
	     '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(setq doc-view-resolution 180)
(add-hook 'doc-view-mode-hook 'asj/disable-special)
(defadvice doc-view-display (after fit-width activate)
  (doc-view-fit-width-to-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account: Personal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mail/use-personal ()
  (interactive)
  (setq user-mail-address (rot13 "nneba@nnebafcynpr.pb.hx")
        user-full-name "Aaron Jackson"
  	mu4e-user-mail-address-list (list user-mail-address))

  (setq mu4e-drafts-folder "/personal/Drafts"
        mu4e-sent-folder "/personal/Sent"
        mu4e-trash-folder "/personal/Trash")

  (setq mu4e-refile-folder
	(concat "/personal/Archive/.Inbox/."
		(format-time-string "%Y" (current-time))))

  (setq mu4e-maildir-shortcuts
        '(("/personal/INBOX" . ?i)
          ("/personal/Sent"  . ?s)
          ("/personal/Junk"  . ?j)))

  (setq mu4e-bookmarks
	'((mu4e-refile-folder "Archive (This Year)" ?a)
	  ("/personal/Lists" "Lists" ?l)
	  ("/personal/Lists/.freebsd" "Lists - freebsd" ?f)
	  ("/personal/Lists/.tuhs" "Lists - TUHS" ?t)
	  ("/personal/Lists/.ham" "Lists - Ham Radio" ?h)
	  ("/personal/Lists/.centos" "Lists - CentOS" ?e)
	  ("/personal/Lists/.groupsio" "Lists - Groups.io" ?g)
	  ("/personal/Lists/.cctalk" "Lists - cctalk" ?c)))

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "escher.rhwyd.co.uk"
        smtpmail-smtp-server "escher.rhwyd.co.uk"
        smtpmail-smtp-service 587))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up account switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mail/use-personal) ;; Use personal by default

(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x m p")
		'(lambda ()
		   (interactive)
		   (mail/use-personal)
		   (message "Account: Personal")
		   (mu4e)))
