;;; http://vxlabs.com/2014/06/06/configuring-emacs-mu4e-with-nullmailer-offlineimap-and-multiple-identities/

(require 'mu4e)

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

;; a  list of user's e-mail addresses
(setq mu4e-user-mail-address-list '("loicfontaine@fastmail.fm"
                                    "ljph.fontaine@gmail.com"
                                    "loicfontaine@valtech.fr"))

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
      mu4e-update-interval 300) ;; update every 5 minutes

(add-hook 'mu4e-index-updated-hook
  (defun lfo-mu4e-new-mail ()
    (start-process "mail-notify" nil "check-new-mails")))

;; set this to nil so signature is not included by default
;; you can include in message with C-c C-w
(setq mu4e-compose-signature-auto-include 't)

;; if you need offline mode, set these -- and create the queue dir
;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
(setq smtpmail-queue-mail  nil
      smtpmail-queue-dir  "~/Maildir/queue/cur")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; display rich-text messages
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(require 'smtpmail)

(defun lfo-mu4e-account-personal ()
  (interactive)
  ;; general emacs mail settings; used when composing e-mail
  ;; the non-mu4e-* stuff is inherited from emacs/message-mode
  (setq user-full-name  "Loïc Fontaine"
        user-mail-address "loicfontaine@fastmail.fm")

  (setq mu4e-compose-signature
        "Loïc Fontaine\nloicfontaine@fastmail.fm\n")

  ;; smtp mail setting
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "mail.messagingengine.com"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465))

(defun lfo-mu4e-account-work ()
  (interactive)
  ;; general emacs mail settings; used when composing e-mail
  ;; the non-mu4e-* stuff is inherited from emacs/message-mode
  (setq user-full-name  "Loïc Fontaine"
        user-mail-address "loicfontaine@valtech.fr")

  (setq mu4e-compose-signature
        "Loïc Fontaine\nloicfontaine@valtech.fr\n")

  ;; smtp mail setting
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "mail.messagingengine.com"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465))

;; quickly change account
(define-key mu4e-main-mode-map (kbd "<f1>") 'lfo-mu4e-account-personal)
(define-key mu4e-main-mode-map (kbd "<f2>") 'lfo-mu4e-account-work)
(define-key mu4e-headers-mode-map (kbd "<f1>") 'lfo-mu4e-account-personal)
(define-key mu4e-headers-mode-map (kbd "<f2>") 'lfo-mu4e-account-work)

;; custom move to trash for fastmail
;; https://groups.google.com/forum/#!topic/mu-discuss/m4ORymDlf0E
(defun lfo-mu4e-move-to-trash ()
  (interactive)
  (mu4e-mark-set 'move "/fastmail/INBOX.Trash"))

(define-key mu4e-headers-mode-map (kbd "d") 'lfo-mu4e-move-to-trash)
(define-key mu4e-view-mode-map (kbd "d") 'lfo-mu4e-move-to-trash)

;; when you reply to a message, use the identity that the mail was sent to
;; -- function that checks to, cc and bcc fields
(defun lfo-mu4e-is-message-to (msg rx)
  "Check if to, cc or bcc field in MSG has any address in RX."
  (or (mu4e-message-contact-field-matches msg :to rx)
      (mu4e-message-contact-field-matches msg :cc rx)
      (mu4e-message-contact-field-matches msg :bcc rx)))

;; we only do something if we recognize something (i.e. no stupid default)
(add-hook 'mu4e-compose-pre-hook
          (defun lfo-mu4e-set-from-address ()
            "Set current identity based on to, cc, bcc of original."
            (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
              (if msg
                  (cond
                   ((lfo-mu4e-is-message-to msg (list "loicfontaine@fastmail.fm"
                                                      "ljph.fontaine@gmail.com"))
                    (lfo-mu4e-account-personal))
                   ((lfo-mu4e-is-message-to msg (list "loicfontaine@valtech.fr"))
                    (lfo-mu4e-account-work)))
                (lfo-mu4e-account-personal)))))

(defun lfo-mu4e-start ()
  (setq server-name "mail")
  (server-start)
  (mu4e))
