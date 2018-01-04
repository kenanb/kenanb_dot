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
"Ensures PACKAGES are installed."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
           (package-install package)
         package)))
   packages))

(or (file-exists-p package-user-dir) (package-refresh-contents))

(ensure-package-installed
 'rainbow-delimiters
 'paredit
 'emms
 'magit
 'dired+
 'counsel
 'yasnippet
 'irony
 'irony-eldoc
 'flycheck
 'flycheck-irony)

;; GLOBAL
(set-frame-font "9x15")
(add-to-list 'default-frame-alist '(font . "9x15"))
(visual-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(set-window-fringes nil 0 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq scroll-step 1
      scroll-conservatively 10000
      inhibit-startup-message t
      auto-window-vscroll nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq auto-save-interval 1000)
(setq auto-save-timeout 60)
(setq vc-make-backup-files t)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backups/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; UTILITY
(require 'eshell)
(require 'dired+)
(require 'magit)
(require 'epa-file)
(require 'uniquify)
(require 'saveplace)
(require 'ivy)
(require 'emms-setup)
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(emms-all)
(emms-default-players)
(epa-file-enable)
(ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(setq compilation-scroll-output 'first-error)
(setq uniquify-buffer-name-style 'forward)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq dired-omit-files "^\\...+$")
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(add-hook 'visual-line-mode-hook
          '(lambda () (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1))))
(put 'dired-find-alternate-file 'disabled nil)
(global-unset-key (kbd "C-<left>")) ; Confusing because behaviour is different it paredit.
(global-unset-key (kbd "C-<right>")) ; Confusing because behaviour is different it paredit.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "RET")     'newline-and-indent)
(global-set-key (kbd "C-c m")   'magit-status)
(global-set-key (kbd "C-c f")   'find-function)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z")     'zap-up-to-char)
;; (global-set-key (kbd "C-s")  'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)
(global-set-key (kbd "C-s")     'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>")    'ivy-resume)
(global-set-key (kbd "M-x")     'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f")  'counsel-describe-function)
(global-set-key (kbd "<f1> v")  'counsel-describe-variable)
(global-set-key (kbd "<f1> l")  'counsel-find-library)
(global-set-key (kbd "<f2> i")  'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u")  'counsel-unicode-char)
(global-set-key (kbd "C-c g")   'counsel-git)
(global-set-key (kbd "C-c j")   'counsel-git-grep)
(global-set-key (kbd "C-c k")   'counsel-ag)
(global-set-key (kbd "C-x l")   'counsel-locate)
(global-set-key (kbd "C-S-o")   'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; DEVELOPMENT
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
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; ;; (load (expand-file-name "~/dev/lisp/clhs-use-local.el") t)
;; (setq browse-url-browser-function
;;       '(("HyperSpec" . eww-browse-url) ("." . browse-url-default-browser)))

(defun irony-mode-counsel-hook ()
  "Set up completion to use irony."
  (define-key irony-mode-map
      [remap completion-at-point] 'counsel-irony)
  (define-key irony-mode-map
      [remap complete-symbol] 'counsel-irony))

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-mode-counsel-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(add-hook 'irony-mode-hook #'irony-eldoc)
(add-hook 'python-mode-hook '(lambda () (eldoc-mode 1)) t)
(load "/usr/share/clang/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.asm$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.z80$" . asm-mode))

;; ORG-MODE
(require 'org)
(require 'org-mouse)
(require 'org-agenda)
;; (require 'ledger-mode)
(setq org-startup-indented t
      org-startup-with-inline-images t
      org-image-actual-width 256
      org-startup-truncated nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-return-follows-link t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-agenda-include-diary t
      org-default-notes-file "~/.notes.org"
      org-agenda-include-all-todo t)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(add-hook 'org-load-hook
          (lambda ()
            (define-key org-mode-map "\C-n" 'org-next-link)
            (define-key org-mode-map "\C-p" 'org-previous-link)))

;; GNUPLOT
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
(global-set-key [(f9)] 'gnuplot-make-buffer)

;; CUSTOM
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(compilation-skip-threshold 2)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(package-selected-packages
   (quote
    (counsel shell-pop rainbow-delimiters paredit magit flycheck emms dired+ better-defaults)))
 '(vc-annotate-background nil)
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types (quote ((undo discard-info)))))
