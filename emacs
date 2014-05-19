;; PACKAGE
(require 'package)
(setq package-archives 
      '(("gnu"      . "http://elpa.gnu.org/packages/")
	("melpa"     . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(defun ensure-package-installed (&rest packages)
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(ensure-package-installed 
 'ac-slime 'auto-complete
 'paredit 'elpy 'better-defaults
 'redshank 'smex 'string-inflection
 'markdown-mode 'magit
 'monokai-theme 'iedit
 'ido 'idomenu 'popup
 'highlight-indentation
 'emms 'pov-mode 'auctex
 'undo-tree 'rainbow-mode
 'rainbow-delimiters 'wgrep
 'litable 'adaptive-wrap
 'shell-pop 'shell-switcher)

;; THEME
(load-theme 'monokai t)
(setq inhibit-splash-screen t)
(show-paren-mode 1)
(setq show-paren-delay 0
      scroll-step 1
      scroll-conservatively 10000
      inhibit-startup-message t
      auto-window-vscroll nil)
(set-default-font "Inconsolata-13")
(column-number-mode 1)
(set-window-fringes nil 0 0)
(global-hl-line-mode t)
(add-hook 'visual-line-mode-hook '(lambda () (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1))))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "RET") 'newline-and-indent)
(defalias 'yes-or-no-p 'y-or-n-p)

;; BUFFER
(require 'dired-x)
(require 'ido)
(ido-mode t)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(defun kill-other-buffers ()
  (interactive)
  (dolist (buff (buffer-list))
    (let ((name (buffer-name buff))
          (wins (get-buffer-window-list buff nil t)))
      (unless (or
               (buffer-modified-p buff)
               (equal buff (current-buffer))
               (= ? (aref name 0))
               (= ?# (aref name 0))
               (= ?* (aref name 0)))
        (kill-buffer buff)
        (dolist(win wins)
          (when (window-live-p win)
            (delete-window win)))))))
(defun make-buffer-uninteresting ()
  (interactive)
  (unless (string-match-p "^ " (buffer-name))
    (rename-buffer (concat " " (buffer-name)))))
(global-set-key (kbd "<f5>") 'make-buffer-uninteresting)

;; COMMON LISP
(require 'cl)
(load (expand-file-name "~/dev/lisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl
               slime-asdf
               slime-fancy
               slime-fuzzy
               slime-banner
               slime-indentation
               slime-media))
(eval-after-load "redshank-loader"
  `(redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t))
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
(require 'ac-slime)
(add-hook 'slime-mode-hook #'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook #'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(add-hook 'slime-mode-hook #'auto-complete-mode)
(add-hook 'slime-repl-mode-hook #'auto-complete-mode)

;; C/C++
(setq-default
 fill-column 79
 tramp-default-method "ssh"
 c-default-style "bsd"
 truncate-lines nil
 tab-width 4
 standard-indent 4
 c-indent-level 4
 c-basic-offset 4)
(add-hook 'c-mode-common-hook '(lambda () (auto-fill-mode 1)))

;; PYTHON
(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)
(add-hook 'python-mode-hook
	  '(lambda () (eldoc-mode 1)) t)

;; POVRAY
(autoload 'pov-mode "pov-mode" t)
(add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))

;; MARKDOWN
(autoload 'markdown-mode "markdown-mode" t)
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; BROWSER
(setq browse-url-generic-program (executable-find "firefox"))
(setq browse-url-browser-function 'browse-url-generic)

;; EMMS
(eval-after-load "emms"
  '(progn
    (require 'emms)
    (require 'emms-player-simple)
    (require 'emms-player-mplayer)
    (require 'emms-source-file)
    (require 'emms-source-playlist)
    (setq
     emms-player-list '(emms-player-mpg321
                        emms-player-ogg123
                        emms-player-mplayer))
    (emms-all)
    (when (file-exists-p "~/doc/music/kenanb.m3u")
      (emms-add-directory-tree "~/doc/music/")
      (emms-shuffle))))
(require 'emms-setup)

;; IRC
(require 'tls)
(require 'erc)
(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
                                     -CAfile ~/.private/certs/CAs.pem
                                     -cert ~/.private/certs/nick.pem
                   \"gnutls-cli --priority secure256
                               --x509cafile ~/.private/certs/CAs.pem
                               --x509certfile ~/.private/certs/nick.pem -p %p %h\"
                   \"gnutls-cli --priority secure256 -p %p %h\""))
(defun start-irc ()
 (interactive)
 (erc-tls :server "irc.flowdock.com" :port 6697
      :nick "kenanb" :full-name "kenanb")
 (erc :server "irc.freenode.net" :port 8002
      :nick "kenanb" :full-name "kenanb")
 (setq erc-autojoin-channels-alist '(("freenode.net" "#lisp" "#lispgames"
                                      "#hy" "#stumpwm" "#concatenative"
                                      "#blender" "#blendercoders"
                                      "#blenderpython" "#yafaray" "#luxrender"))))
