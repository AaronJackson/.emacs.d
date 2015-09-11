;; NOTES: To protect from spam, I have rot13 the email addresses in
;;        this file. The script will decode these though.  The script
;;        allows for multiple mail accounts to operate by calling
;;        mail/pls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smtpmail)
(setq mu4e-get-mail-command "true"
      mu4e-update-interval 60
      mu4e-html2text-command "html2text -utf8 -width 72"
      mu4e-msg2pdf "/usr/bin/msg2pdf" ;; requires maildir-utils-extra
      mu4e-maildir "~/Maildir"
      message-kill-buffer-on-exit t
      mu4e-sent-messages-behavior 'sent)

(defun mail/mu4e-default-hook ()
  (setq show-trailing-whitespace nil)
  (linum-mode -1))

(add-hook 'mu4e-headers-mode-hook 'mail/mu4e-default-hook)
(add-hook 'mu4e-compose-mode-hook 'mail/mu4e-default-hook)
(add-hook 'mu4e-view-mode-hook    'mail/mu4e-default-hook)

(setq mu4e-compose-complete-only-after
      (format-time-string
       "%Y-%m-%d" ;; I only want past six months
       (time-subtract (current-time)
		      (seconds-to-time 15552000))))

(setq mu4e-headers-fields
      '((:date          .  25)
	(:flags         .   6)
	(:from-or-to    .  22)
	(:subject       .  nil))
      mu4e-headers-from-or-to-prefix nil)

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
	(concat "/personal/Archive.Inbox."
		(format-time-string "%Y" (current-time))))

  (setq mu4e-maildir-shortcuts
        '(("/personal/INBOX" . ?i)
          ("/personal/Sent"  . ?s)
          ("/personal/Junk"  . ?j)))

  (setq mu4e-bookmarks
	'((mu4e-refile-folder "Archive (This Year)" ?a)))

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "mail.tradead.co.uk"
        smtpmail-smtp-server "mail.tradead.co.uk"
        smtpmail-smtp-service 587))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account: University of Nottingham
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mail/use-nottingham ()
  (interactive)
  (setq user-mail-address (rot13 "cfknfw@abggvatunz.np.hx")
        user-full-name "Aaron Jackson"
	mu4e-user-mail-address-list (list user-mail-address))

  (setq mu4e-drafts-folder "/nottingham/Drafts"
        mu4e-sent-folder "/nottingham/Sent Items"
        mu4e-trash-folder "/nottingham/Deleted Items"
	mu4e-refile-folder "/nottingham/Archive")

  (setq mu4e-maildir-shortcuts
        '(("/nottingham/INBOX" . ?i)
          ("/nottingham/Sent Items"  . ?s)
          ("/nottingham/Junk E-Mail"  . ?j)))

  (setq mu4e-bookmarks
	'(("flag:unread AND maildir:/nottingham/INBOX*"
	   "Unread Email" ?u)
	  ("maildir:/nottingham/Archive"
	   "Archived Email" ?a)))

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'ssl
        smtpmail-default-smtp-server "smtp.nottingham.ac.uk"
        smtpmail-smtp-server "smtp.nottingham.ac.uk"
        smtpmail-smtp-service 465))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up account switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mail/use-personal) ;; Use personal by default

(defun mail/pls (a)
  (interactive "cAccount: [p]ersonal, [n]ottingham")
  (pcase a
    (?p (mail/use-personal))
    (?n (mail/use-nottingham)))
  (mu4e))
(global-set-key [f9] 'mail/pls)


