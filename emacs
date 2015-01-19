;; PACKAGE
(require 'package)
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp/")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(setq package-archives 
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages")))
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
 'ac-slime 'auto-complete 'paredit 'elpy 'better-defaults 'wgrep
 'redshank 'string-inflection 'markdown-mode 'magit 'imenu 'flycheck
 'lua-mode 'helm 'highlight-indentation 'iedit 'twittering-mode
 'emms 'pov-mode 'auctex 'undo-tree 'rainbow-mode 'monokai-theme
 'litable 'adaptive-wrap 'rainbow-delimiters 'shell-pop 'ggtags
 'shell-switcher 'dired+ 'popwin 'popup 'command-log-mode 'help-fns+
 'latex-preview-pane 'smart-mode-line)

;; GLOBAL
(require 'dired+)
(require 'shell-pop)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'smart-mode-line)
(require 'epa-file)
(epa-file-enable)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(load-theme 'monokai t)
(sml/setup)
(setq org-replace-disputed-keys t) ; only works if set before Helm loads org.el.
(autoload 'gtags-mode "gtags" "" t)
(ac-config-default)
(global-auto-complete-mode t)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(show-paren-mode 1)
(visual-line-mode 1)
(setq show-paren-delay 0
      scroll-step 1
      scroll-conservatively 10000
      inhibit-startup-message t
      auto-window-vscroll nil)
(set-frame-parameter (selected-frame) 'alpha '(99 99))
(add-to-list 'default-frame-alist '(alpha 99 99))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(set-default-font "6x13")
(add-to-list 'default-frame-alist '(font . "6x13"))
(column-number-mode 1)
(set-window-fringes nil 0 0)
(global-hl-line-mode 1)
(setq compilation-scroll-output t)
;; (setq compilation-scroll-output 'first-error)
(add-hook 'visual-line-mode-hook '(lambda () (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1))))
(global-set-key (kbd "RET") 'newline-and-indent)
(global-unset-key (kbd "C-z")) ; useless in GUI mode.
(global-set-key (kbd "C-z C-c") 'compile)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c f") 'find-function)
(defalias 'yes-or-no-p 'y-or-n-p)
;; Put autosaves in their place (not in current dir)
(defvar autosave-dir "~/.emacs.d/autosaves/")
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq auto-save-interval 1000)
(setq auto-save-timeout 60)
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
;; windmove config
(windmove-default-keybindings)
(setq windmove-wrap-around t)
;;; shrink/enlarge windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; BUFFER SWITCHING
(defun regexp-next-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (next-buffer))))
(defun regexp-previous-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (previous-buffer))))
(global-set-key [remap next-buffer] 'regexp-next-buffer)
(global-set-key [remap previous-buffer] 'regexp-previous-buffer)

;; GNUS
(setq gnus-directory "~/.emacs.d/news/")
(setq message-directory "~/.emacs.d/mail/")
(setq nnfolder-directory "~/.emacs.d/mail/archive")
(defun gnus-switch-to-group-buffer ()
  "Switch to gnus group buffer if it exists, otherwise start gnus."
  (interactive)
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (switch-to-buffer-other-window gnus-group-buffer)
    (gnus)))
(global-set-key (kbd "C-x g") 'gnus-switch-to-group-buffer)

;; HELM
(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(helm-mode 1)
(helm-adaptative-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)
(add-hook 'org-mode-hook
  (lambda ()
    (define-key org-mode-map (kbd "C-x c o h") #'helm-org-headlines)))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)


;; POPWIN
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-z") popwin:keymap)
(setq display-buffer-function 'popwin:display-buffer)
;; Helm in popwin
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config) 
;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)
;; M-!
(push "*Shell Command Output*" popwin:special-display-config)
;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)
;; slime
(push "*slime-apropos*" popwin:special-display-config)
(push "*slime-macroexpansion*" popwin:special-display-config)
(push "*slime-description*" popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push "*slime-xref*" popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push 'slime-repl-mode popwin:special-display-config)
(push 'slime-connection-list-mode popwin:special-display-config)
;; vc
(push "*vc-diff*" popwin:special-display-config)
(push "*vc-change-log*" popwin:special-display-config)
;; undo-tree
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
;; Gnus
(push 'gnus-group-mode popwin:special-display-config)

;; LISP
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
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
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
;; (load (expand-file-name "~/dev/lisp/clhs-use-local.el") t)
(setq browse-url-browser-function
      '(("HyperSpec" . eww-browse-url) ("." . browse-url-default-browser)))

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
(add-hook 'c-mode-common-hook #'ggtags-mode)
(add-hook 'c-mode-common-hook '(lambda () (auto-fill-mode 1)))

;; PYTHON
(elpy-enable)
(elpy-use-ipython)
(add-hook 'python-mode-hook #'ggtags-mode)
(add-hook 'python-mode-hook
	  '(lambda () (eldoc-mode 1)) t)

;; LUA
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 4)
(add-hook 'lua-mode-hook #'auto-complete-mode)
(add-hook 'lua-mode-hook #'(lambda () (gtags-mode 1)))
(add-hook 'lua-mode-hook #'ggtags-mode)
(add-hook 'lua-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-gtags)))

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

;; LATEX
(latex-preview-pane-enable)
(defun TeX-fold-and-PDF-on () (progn (TeX-fold-mode 1)
                                     (TeX-PDF-mode-on)))
(add-hook 'LaTeX-mode-hook #'TeX-fold-and-PDF-on)
(add-hook 'TeX-mode-hook #'TeX-fold-and-PDF-on)

;; ORG-MODE
(require 'org)
(require 'ox-latex)
(require 'org-mouse)
(require 'ledger-mode)
(require 'ob-lua)
(setq org-startup-indented t
      org-startup-with-inline-images t
      org-image-actual-width 256
      org-startup-truncated nil
      org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-return-follows-link t
      org-hide-emphasis-markers t
      org-pretty-entities t)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary t)
(defun org-tangle-run-linux ()
  (interactive)
  (make-local-variable 'compilation-read-command)
  (setq compilation-read-command nil)
  (org-babel-tangle (current-buffer))
  (compile "cocos run -p linux"))
(add-hook 'org-mode-hook
  (lambda ()
    (define-key org-mode-map (kbd "C-z C-c") 'org-tangle-run-linux)))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(add-hook 'org-load-hook
          (lambda ()
            (define-key org-mode-map "\C-n" 'org-next-link)
            (define-key org-mode-map "\C-p" 'org-previous-link)))
(add-to-list 'org-latex-classes
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
          '("koma-book"
             "\\documentclass{scrbook}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; GNUPLOT
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
(global-set-key [(f9)] 'gnuplot-make-buffer)

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
    (when (file-exists-p "~/doc/media/music/music.list")
      (emms-add-directory-tree "~/doc/media/music/")
      (emms-shuffle))))
(require 'emms-setup)

;; CUSTOM
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:inherit org-meta-line :background "gray20" :foreground "#75715E" :box (:line-width 1 :color "grey75") :slant italic))) t)
 '(org-block-end-line ((t (:inherit org-meta-line))) t)
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 1.1))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 1.0))))
 '(org-link ((t (:background "dark slate blue" :foreground "gainsboro" :box (:line-width 2 :color "grey75" :style released-button) :underline nil :family "UbuntuMono"))))
 '(org-table ((t (:background "gray20" :foreground "#A6E22E" :box (:line-width 1 :color "grey75"))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(compilation-message-face (quote default))
 '(doc-view-continuous t)
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (awk . t)
     (C . t)
     (calc . t)
     (ditaa . t)
     (gnuplot . t)
     (latex . t)
     (ledger . t)
     (lisp . t)
     (lua . t)
     (makefile . t)
     (maxima . t)
     (org . t)
     (python . t)
     (scheme . t)
     (sh . t)
     (sqlite . t))))
 '(org-ditaa-eps-jar-path "/usr/share/java/ditaa-eps/DitaaEps.jar")
 '(org-ditaa-jar-path "/usr/share/java/ditaa/ditaa.jar")
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s")
     ("\\.xcf\\'" . "gimp %s")
     ("\\.psd\\'" . "gimp %s")
     ("\\.kra\\'" . "krita %s")
     ("\\.ora\\'" . "krita %s")
     ("\\.blend\\'" async-shell-command
      (concat "blender " file)))))
 '(org-html-html5-fancy t)
 '(send-mail-function (quote mailclient-send-it))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-height 15)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
