;; PACKAGE
(require 'package)
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp/")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(setq package-archives 
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")))
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
 'ac-slime           'auto-complete     'paredit               'elpy
 'better-defaults    'wgrep             'redshank              'string-inflection
 'markdown-mode      'magit             'imenu                 'flycheck
 'lua-mode           'helm              'highlight-indentation 'iedit
 'emms               'auctex            'undo-tree             'rainbow-mode
 'shell-pop          'ggtags            'shell-switcher        'dired+
 'popwin             'popup             'command-log-mode      'help-fns+
 'latex-preview-pane 'flymake-json      'fold-dwim-org         'adaptive-wrap
 'litable            'rainbow-delimiters)

;; GLOBAL
(require 'dired+)
(require 'shell-pop)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'epa-file)
(setq indent-tabs-mode nil)
(epa-file-enable)
(tool-bar-mode 0)
(menu-bar-mode 0)
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
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(load-theme 'misterioso)
(set-default-font "6x13")
(set-default-font "Misc Tamsyn-10")
(add-to-list 'default-frame-alist '(font . "Misc Tamsyn-8"))
;; (set-default-font "Ubuntu Mono")
;; (add-to-list 'default-frame-alist '(font . "Ubuntu Mono"))
(column-number-mode 1)
(set-window-fringes nil 0 0)
(global-hl-line-mode 0)
;; (setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)
(add-hook 'visual-line-mode-hook '(lambda () (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1))))
(global-set-key (kbd "RET") 'newline-and-indent)
(global-unset-key (kbd "C-z")) ; useless in GUI mode.
(global-set-key (kbd "C-z C-c") 'compile)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c f") 'find-function)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq magit-last-seen-setup-instructions "1.4.0")
;; Put autosaves in their place (not in current dir)
(defvar autosave-dir "~/.emacs.d/autosaves/")
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq auto-save-interval 1000)
(setq auto-save-timeout 60)
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; windmove config
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(global-unset-key (kbd "C-<left>")) ; Confusing because behaviour is different it paredit.
(global-unset-key (kbd "C-<right>")) ; Confusing because behaviour is different it paredit.
;;; shrink/enlarge windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(global-set-key (kbd "<f5>") 'string-inflection-all-cycle)

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
(helm-adaptive-mode t)
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
               slime-c-p-c
               slime-banner
               slime-fancy-inspector
               slime-references
               slime-xref-browser
               ;; slime-highlight-edits
               slime-scratch
               slime-trace-dialog
               slime-sprof
               slime-indentation
               slime-media))
(setq slime-scratch-file "~/dev/lisp/local-projects/scratch.lisp")
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(setq slime-fuzzy-completion-in-place t)
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
(add-hook 'slime-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-slime-fuzzy)))
(add-hook 'slime-repl-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-slime-fuzzy)))
;; (load (expand-file-name "~/dev/lisp/clhs-use-local.el") t)
(setq browse-url-browser-function
      '(("HyperSpec" . eww-browse-url) ("." . browse-url-default-browser)))

;; C/C++
(global-ede-mode 1)
(require 'semantic/sb)
(semantic-mode 1)
(require 'fold-dwim-org)
(load "/usr/share/clang/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)
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
(add-hook 'c-mode-common-hook #'hs-minor-mode)
(add-hook 'c-mode-common-hook #'fold-dwim-org/minor-mode)
(add-hook 'c-mode-common-hook '(lambda () (auto-fill-mode 1)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; ASM
(add-to-list 'auto-mode-alist '("\\.asm$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.z80$" . asm-mode))

;; PYTHON
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
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
(add-to-list 'load-path "/usr/share/asymptote/")
(load-file "/usr/share/asymptote/asy-init.el")
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
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(setq org-default-notes-file "~/.notes.org")
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(add-hook 'org-load-hook
          (lambda ()
            (define-key org-mode-map "\C-n" 'org-next-link)
            (define-key org-mode-map "\C-p" 'org-previous-link)))

;; JS
(global-set-key (kbd "C-c j v") 'flymake-json-load)
(global-set-key (kbd "C-c j s") 'elpy-flymake-show-error)

;; ESHELL
(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))

(add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))
(add-to-list 'ac-modes 'eshell-mode)


;; GNUPLOT
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
(global-set-key [(f9)] 'gnuplot-make-buffer)

;; EMMS
(require 'emms-setup)
(emms-all)
(emms-default-players)
(eval-after-load "emms"
  (when (file-exists-p "~/doc/media/music/music.list")
    (emms-add-directory-tree "~/doc/media/music/")
    (emms-shuffle)))

;; CUSTOM
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:inherit org-meta-line :background "gray20" :foreground "#75715E" :box (:line-width 1 :color "grey75") :slant italic))) t)
 '(org-block-end-line ((t (:inherit org-meta-line))) t)
 '(org-link ((t (:background "dark slate blue" :foreground "gainsboro" :box (:line-width 2 :color "grey75" :style released-button) :underline nil :family "UbuntuMono"))))
 '(org-table ((t (:background "gray20" :foreground "#A6E22E" :box (:line-width 1 :color "grey75")))))
 '(slime-highlight-edits-face ((t (:background "black")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(compilation-skip-threshold 2)
 '(doc-view-continuous t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-babel-load-languages
   (quote
	((emacs-lisp . t)
	 (asymptote . t)
	 (awk . t)
	 (C . t)
	 (calc . t)
	 (dot . t)
	 (gnuplot . t)
	 (latex . t)
	 (ledger . t)
	 (lisp . t)
	 (makefile . t)
	 (maxima . t)
	 (org . t)
	 (python . t)
	 (scheme . t)
	 (sh . t)
	 (sqlite . t))))
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
 '(org-latex-classes
   (quote
	(("extarticle" "\\documentclass{extarticle}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	  ("\\paragraph{%s}" . "\\paragraph*{%s}")
	  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 ("koma-book" "\\documentclass{scrbook}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	  ("\\paragraph{%s}" . "\\paragraph*{%s}")
	  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 ("koma-article" "\\documentclass{scrartcl}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	  ("\\paragraph{%s}" . "\\paragraph*{%s}")
	  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 ("article" "\\documentclass[11pt]{article}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	  ("\\paragraph{%s}" . "\\paragraph*{%s}")
	  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 ("report" "\\documentclass[11pt]{report}"
	  ("\\part{%s}" . "\\part*{%s}")
	  ("\\chapter{%s}" . "\\chapter*{%s}")
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	 ("book" "\\documentclass[11pt]{book}"
	  ("\\part{%s}" . "\\part*{%s}")
	  ("\\chapter{%s}" . "\\chapter*{%s}")
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(package-selected-packages
   (quote
	(org ecb wgrep undo-tree string-inflection shell-switcher shell-pop redshank rainbow-mode rainbow-delimiters popwin paredit markdown-mode magit lua-mode litable latex-preview-pane iedit help-fns+ helm ggtags fold-dwim-org flymake-json flycheck emms elpy dired+ command-log-mode better-defaults auctex adaptive-wrap ac-slime)))
 '(safe-local-variable-values
   (quote
	((Package . FOSSICKER)
	 (Package . CL-USER)
	 (Syntax . COMMON-LISP))))
 '(send-mail-function (quote mailclient-send-it))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-size 15)
 '(vc-annotate-background nil)
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types (quote ((undo discard-info)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
