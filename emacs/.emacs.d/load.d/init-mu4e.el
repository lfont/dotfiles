;;; init-mu4e.el --- Mail client
;;; Commentary:
;;; http://vxlabs.com/2014/06/06/configuring-emacs-mu4e-with-nullmailer-offlineimap-and-multiple-identities/
;;; Code:

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :commands mu4e
  :bind (:map mu4e-main-mode-map
              ("<f1>" . my/mu4e-account-fastmail)
              ("<f2>" . my/mu4e-account-fasterize)
         :map mu4e-headers-mode-map
              ("<f1>" . my/mu4e-account-fastmail)
              ("<f2>" . my/mu4e-account-fasterize)
              ("d" . my/mu4e-move-to-trash)
         :map mu4e-view-mode-map
              ("d" . my/mu4e-move-to-trash))
  :init
  ;; basic user information
  (setq user-full-name  "Loïc Fontaine")

  ;; a  list of user's e-mail addresses
  (setq mu4e-user-mail-address-list '("loicfontaine@fastmail.fm"
                                      "ljph.fontaine@gmail.com"
                                      "channary.loic@gmail.com"
                                      "l@fasterize.com"
                                      "loic@fasterize.com"))

  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
        '((:date    . 25)
          (:flags   .  6)
          (:from    . 22)
          (:subject . nil)))

  ;; If you get your mail without an explicit command,
  ;; use "true" for the command (this is the default)
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300 ;; update every 5 minutes
        mu4e-hide-index-messages t)

  ;; set this to nil so signature is not included by default
  ;; you can include in message with C-c C-w
  (setq mu4e-compose-signature-auto-include 't)

  ;; if you need offline mode, set these -- and create the queue dir
  ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
  (setq smtpmail-queue-mail  nil
        smtpmail-queue-dir  "~/Maildir/queue/cur")

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; new mail query
  (setq my/mu4e-new-mail-query
        (concat
         "flag:unread"
         " AND NOT flag:trashed")
        my/mu4e-account-new-mail-query nil)
  :config
  ;; custom bookmarks
  (add-to-list 'mu4e-bookmarks
               '(my/mu4e-account-new-mail-query
                 "New messages"
                 ?n))

  ;; new mail notification
  (use-package mu4e-alert
    :ensure t
    :demand t
    :init
    (setq alert-fade-time 10
          mu4e-alert-interesting-mail-query my/mu4e-new-mail-query
          mu4e-alert-email-notification-types '(subjects))
    :config
    (mu4e-alert-set-default-style 'libnotify)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display))

  ;; display rich-text messages
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; send mail
  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; custom move to trash
  ;; https://groups.google.com/forum/#!topic/mu-discuss/m4ORymDlf0E
  (defun my/mu4e-move-to-trash ()
    (interactive)
    (mu4e-mark-set 'move mu4e-trash-folder))

  ;; account setup
  (defun my/mu4e-account-fastmail ()
    (interactive)
    (message "Switching to fastmail account...")

    (setq my/mu4e-account-new-mail-query (concat my/mu4e-new-mail-query
                                                 " AND maildir:/fastmail/*"
                                                 " AND NOT maildir:\"/fastmail/INBOX.Junk Mail\""
                                                 " AND NOT maildir:/fastmail/INBOX.Trash"))

    ;; the next are relative to `mu4e-maildir'
    ;; instead of strings, they can be functions too, see
    ;; their docstring or the chapter 'Dynamic folders'
    (setq mu4e-sent-folder   "/fastmail/INBOX.Sent Items"
          mu4e-drafts-folder "/fastmail/INBOX.Drafts"
          mu4e-trash-folder  "/fastmail/INBOX.Trash")

    ;; the maildirs you use frequently; access them with 'j' ('jump')
    (setq mu4e-maildir-shortcuts
          '(("/fastmail/INBOX"            . ?i)
            ("/fastmail/INBOX.Archive"    . ?a)
            ("/fastmail/INBOX.Sent Items" . ?s)
            ("/fastmail/INBOX.Notes"      . ?n)))

    ;; general emacs mail settings; used when composing e-mail
    ;; the non-mu4e-* stuff is inherited from emacs/message-mode
    (setq user-mail-address "loicfontaine@fastmail.fm"
          mu4e-compose-signature (concat user-full-name
                                         "\nloicfontaine@fastmail.fm"
                                         "\n"))

    ;; smtp mail setting
    (setq smtpmail-smtp-server "mail.messagingengine.com"
          smtpmail-stream-type 'ssl
          smtpmail-smtp-service 465))

  (defun my/mu4e-account-fasterize ()
    (interactive)
    (message "Switching to fasterize account...")

    (setq my/mu4e-account-new-mail-query (concat my/mu4e-new-mail-query
                                                 " AND maildir:/fasterize/*"
                                                 " AND NOT maildir:/fasterize/[Gmail].Spam"
                                                 " AND NOT maildir:\"/fasterize/[Gmail].All Mail\""
                                                 " AND NOT maildir:/fasterize/[Gmail].Important"
                                                 " AND NOT maildir:/fasterize/reading"))

    ;; the next are relative to `mu4e-maildir'
    ;; instead of strings, they can be functions too, see
    ;; their docstring or the chapter 'Dynamic folders'
    (setq mu4e-sent-folder   "/fasterize/[Gmail].Sent Mail"
          mu4e-drafts-folder "/fasterize/[Gmail].Drafts"
          mu4e-trash-folder  "/fasterize/[Gmail].Trash")

    ;; the maildirs you use frequently; access them with 'j' ('jump')
    (setq mu4e-maildir-shortcuts
          '(("/fasterize/INBOX"             . ?i)
            ("/fasterize/[Gmail].All Mail"  . ?a)
            ("/fasterize/[Gmail].Sent Mail" . ?s)))

    ;; general emacs mail settings; used when composing e-mail
    ;; the non-mu4e-* stuff is inherited from emacs/message-mode
    (setq user-mail-address "l@fasterize.com"
          mu4e-compose-signature (concat user-full-name
                                         "\nFasterize"
                                         "\n"))

    ;; smtp mail setting
    (setq smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-stream-type 'ssl
          smtpmail-smtp-service 465))

  ;; when you reply to a message, use the identity that the mail was sent to
  ;; -- function that checks to, cc and bcc fields
  (defun my/mu4e-is-message-to (msg rx)
    "Check if to, cc or bcc field in MSG has any address in RX."
    (or (mu4e-message-contact-field-matches msg :to rx)
        (mu4e-message-contact-field-matches msg :cc rx)
        (mu4e-message-contact-field-matches msg :bcc rx)))

  ;; we only do something if we recognize something (i.e. no stupid default)
  (defun my/mu4e-set-from-address ()
    "Set current identity based on to, cc, bcc of original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (if msg
          (cond
           ((my/mu4e-is-message-to msg (list "loicfontaine@fastmail.fm"
                                             "ljph.fontaine@gmail.com"
                                             "channary.loic@gmail.com"))
            (my/mu4e-account-fastmail))
           ((my/mu4e-is-message-to msg (list "l@fasterize.com"
                                             "loic@fasterize.com"))
            (my/mu4e-account-fasterize))))))

  (add-hook 'mu4e-compose-pre-hook 'my/mu4e-set-from-address)

  ;; set default account
  (my/mu4e-account-fastmail))

(provide 'init-mu4e)

;;; init-mu4e.el ends here
