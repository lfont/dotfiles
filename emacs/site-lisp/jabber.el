;; https://fasterize.hipchat.com/account/xmpp
(require 'jabber)

(setq jabber-account-list '(("139514_2672027@chat.hipchat.com"
                              (:port . 5222)
                              (:connection-type . starttls))))

(setq jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30)

(setq jabber-alert-presence-message-function
      (lambda (who oldstatus newstatus statustext) nil))

(require 'notify)

(defun my/jabber-notify-alert-message (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via notify.el"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (notify (format "(PM) %s" (jabber-jid-displayname (jabber-jid-user from)))
                (format "%s: %s" (jabber-jid-resource from) text))
        (notify (format "%s" (jabber-jid-displayname from)) text))))

(add-hook 'jabber-alert-message-hooks 'my/jabber-notify-alert-message)

(defun my/jabber-notify-alert-muc (nick group buffer text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat group messages via notify.el"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buffer))))
    (if nick
        (when (or jabber-muc-alert-self
                  (not (string= nick (cdr (assoc group *jabber-active-groupchats*)))))
          (notify (format "%s@%s" nick (jabber-jid-displayname group)) text))
        (notify (format "%s" (jabber-jid-displayname group)) text))))

(add-hook 'jabber-alert-muc-hooks 'my/jabber-notify-alert-muc)

;; this info is present on the hipchat xmpp info page
(defvar my/jabber-hipchat-room-list '(
            ("fasterize" . "139514_fasterize")
            ("tech" . "139514_tech")))

;; To join HipChat rooms easily
(defun my/jabber-hipchat-join ()
  (interactive)
  (let* ((room-list (sort (mapcar 'car my/jabber-hipchat-room-list) 'string-lessp))
         (selected-room (completing-read "Room name: " room-list))
         (hipchat-mapping (cdr (assoc selected-room my/jabber-hipchat-room-list))))
    (jabber-groupchat-join
     (jabber-read-account)
     (concat hipchat-mapping "@conf.hipchat.com")
     "Loïc Fontaine"
     t)))

;;; function to start jabber
(defun my/jabber ()
  (interactive)
  (setq server-name "jabber")
  (server-start)
  (setq initial-buffer-choice (lambda ()
                                (jabber-display-roster)
                                (get-buffer "*-jabber-roster-*")))
  (jabber-connect-all))

(provide 'my/jabber)
