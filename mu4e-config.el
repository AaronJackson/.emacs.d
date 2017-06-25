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
      ;; mu4e-html2text-command "w3m -dump -cols 70 -T text/html"
      mu4e-html2text-command "html2text -utf8 -width 72 -style compact"
;      mu4e-html2text-command 'mu4e-shr2text
;      mu4e-view-prefer-html t
      mu4e-msg2pdf "/usr/bin/msg2pdf" ;; requires maildir-utils-extra
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

;; (setq
;;   mu4e-index-cleanup nil      ;; don't do a full cleanup check
;;   mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs

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
	  ("/personal/Lists/.cctalk" "Lists - cctalk" ?c)))

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "escher.rhwyd.co.uk"
        smtpmail-smtp-server "escher.rhwyd.co.uk"
        smtpmail-smtp-service 587))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account: University of Nottingham
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mail/use-nottingham ()
  (interactive)
  (setq user-mail-address (rot13 "cfknfw@abggvatunz.np.hx")
        user-full-name "Aaron Jackson"
	mu4e-user-mail-address-list (list user-mail-address
					  (rot13 "nneba.wnpxfba@abggvatunz.np.hx")
					  (rot13 "nfw@pf.abgg.np.hx")
					  (rot13 "cfknfw@rkznvy.abggvatunz.np.hx")))

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

(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x m p")
		'(lambda ()
		   (interactive)
		   (mail/use-personal)
		   (message "Account: Personal")
		   (mu4e)))
(global-set-key (kbd "C-x m n")
		'(lambda ()
		   (interactive)
		   (mail/use-nottingham)
		   (message "Account: University of Nottingham")
		   (mu4e)))
