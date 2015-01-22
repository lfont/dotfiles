(require 'jabber)

(setq jabber-account-list '(("lfontaine@mappyim"
                              (:network-server . "mappyim")
                              (:port . 5223)
                              (:connection-type . ssl))))

(setq jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30)

(setq jabber-alert-presence-message-function
      (lambda (who oldstatus newstatus statustext) nil))

(require 'notify)

(defun notify-jabber-alert-message (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via notify.el"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (notify (format "(PM) %s" (jabber-jid-displayname (jabber-jid-user from)))
                (format "%s: %s" (jabber-jid-resource from) text))
        (notify (format "%s" (jabber-jid-displayname from)) text))))

(add-hook 'jabber-alert-message-hooks 'notify-jabber-alert-message)

(defun notify-jabber-alert-muc (nick group buffer text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat group messages via notify.el"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buffer))))
    (if nick
        (when (or jabber-muc-alert-self
                  (not (string= nick (cdr (assoc group *jabber-active-groupchats*)))))
          (notify (format "%s@%s" nick (jabber-jid-displayname group)) text))
        (notify (format "%s" (jabber-jid-displayname group)) text))))

(add-hook 'jabber-alert-muc-hooks 'notify-jabber-alert-muc)

(defun lfo-jabber-start ()
  (setq initial-buffer-choice (lambda ()
                                (jabber-display-roster)
                                (get-buffer "*-jabber-roster-*")))
  (jabber-connect-all))
