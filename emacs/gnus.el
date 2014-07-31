;; Choose plaintext every time this is possible.
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Group display format
(setq gnus-permanently-visible-groups ".*")
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Don't hide messages that have been read
(setq gnus-fetch-old-headers t)

;; Sort threads by date, rather than the order the messages appear in
;; the folder...
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date))

;; The Insidious Big Brother Database.
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb-always-add-addresses t)
(setq bbdb-send-mail-style 'gnus)
(setq bbdb-complete-name-full-completion t)
(setq bbdb-completion-type 'primary-or-name)
(setq bbdb-complete-name-allow-cycling t)

;; Automatically refresh gnus mail groups
(require 'gnus-demon)
(setq gnus-use-demon t)
(gnus-demon-add-handler 'gnus-group-get-new-news 5 t)

;; Display notification on new news
(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)
(gnus-demon-add-scanmail)

;; Configure accounts
(setq user-full-name    "Loïc Fontaine"
      user-mail-address "ljph.fontaine@gmail.com")

(setq gnus-select-method
    '(nntp "news.gmane.org"))

(require 'nnir) ; To be able to search for mail in imap
(setq gnus-secondary-select-methods
    '((nnimap "work"
              (nnimap-address "imap.mappy.com")
              (nnimap-server-port 143)
              (nnimap-stream starttls)
              (nnir-search-engine imap))
      (nnimap "home"
              (nnimap-address "imap.gmail.com")
              (nnimap-server-port 993)
              (nnimap-stream tls)
              (nnir-search-engine imap))))

;; Let Gnus change the "From:" line by looking at current group we are in
(setq gnus-parameters
    '(("home"
        (display . all)
        (posting-style
            (address "ljph.fontaine@gmail.com")))
      ("work"
        (display . all)
        (posting-style
          (address "loic.fontaine.ext@mappy.com")))))

;; Available SMTP accounts. The format is
;; type of connection - account in the from field - smtp server -
;; port - login name - password. You can leave the password field
;; as NIL and emacs will ask every time
(defvar smtp-accounts
    '((ssl "ljph.fontaine@gmail.com"     "smtp.gmail.com" 587 "ljph.fontaine@gmail.com" nil)
      (ssl "loic.fontaine.ext@mappy.com" "smtp.mappy.com" 587 "lfontaine"               nil)))

;; Now lets configure smtpmail.el with your name and functions to send
;; mail using your smtp accounts by changing the from field
(require 'cl)
(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
    message-send-mail-function 'smtpmail-send-it
    mail-from-style nil
    user-full-name "Loïc Fontaine"
    smtpmail-debug-info t
    smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
    "Set related SMTP variables for supplied parameters."
    (setq smtpmail-smtp-server server
          smtpmail-smtp-service port
          smtpmail-auth-credentials (list (list server port user password))
          smtpmail-auth-supported (list mech)
          smtpmail-starttls-credentials nil)
    (message "Setting SMTP server to `%s:%s' for user `%s'."
             server port user))

(defun set-smtp-ssl (server port user password &optional key cert)
    "Set related SMTP and SSL variables for supplied parameters."
    (setq starttls-use-gnutls t
          starttls-gnutls-program "gnutls-cli"
          starttls-extra-arguments nil
          smtpmail-smtp-server server
          smtpmail-smtp-service port
          smtpmail-auth-credentials (list (list server port user password))
          smtpmail-starttls-credentials (list (list server port key cert)))
    (message "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
             server port user))

(defun change-smtp ()
    "Change the SMTP server according to the current from line."
    (save-excursion
        (loop with from = (save-restriction
                            (message-narrow-to-headers)
                            (message-fetch-field "from"))
            for (auth-mech address . auth-spec) in smtp-accounts
            when (string-match address from)
            do (cond
                ((memq auth-mech '(cram-md5 plain login))
                 (return (apply 'set-smtp (cons auth-mech auth-spec))))
                ((eql auth-mech 'ssl)
                 (return (apply 'set-smtp-ssl auth-spec)))
                (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
            finally (error "Cannot infer SMTP information."))))

;; Trigger CHANGE-SMTP before every SMTPMAIL-VIA-SMTP call
(defadvice smtpmail-via-smtp
    (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
    "Call `change-smtp' before every `smtpmail-via-smtp'."
    (with-current-buffer smtpmail-text-buffer (change-smtp)))

(ad-activate 'smtpmail-via-smtp)
