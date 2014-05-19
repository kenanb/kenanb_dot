(setq user-mail-address "kenanbolukbasi@gmail.com")
(setq user-full-name "Kenan Bölükbaşı")

(setq gnus-select-method '(nntp "news.gmane.org"))

(add-to-list 'gnus-secondary-select-methods 
	     '(nnimap "gmail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
				   "kenanbolukbasi@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq message-directory "~/doc/storage/mail/")
(setq gnus-directory "~/doc/storage/news/")

(setq bbdb/news-auto-create-p t)

(setq gnus-add-to-list t
      gnus-interactive-exit nil
      gnus-save-newsrc-file nil
      gnus-save-killed-list nil)
