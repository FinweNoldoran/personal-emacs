(require 'package) ;; You might already have this line

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;comple emacs directory
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(package-initialize)
(eval-when-compile
  (require 'use-package))

(setq inhibit-startup-screen t)

(setq explicit-shell-file-name "/usr/local/bin/zsh")

;;recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; reftex helm compat

(eval-after-load 'helm-mode '(add-to-list
			      'helm-completing-read-handlers-alist '(reftex-citation . nil) )
		 )

;;magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; get correct terminal

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;hide instead of close
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame"
  (let ((frame   (posn-window (event-start event)))
        (numfrs  (length (frame-list))))
    (if (> numfrs 1)
	ad-do-it
      (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

;;helm
(use-package helm
  :init
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x))

;; indent
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;;rainbow delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; powerline
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
;;(require 'powerline)
;;(setq powerline-arrow-shape 'curve)

;; font
(set-face-attribute 'default nil :foundry "apple" :family "Sauce Code Powerline" :height 140)

;; theme
(if window-system
    (setq solarized-high-contrast-mode-line t)) ;;required for svg modeline.

;;theme
(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'monokai t))

;; svg mode line
(add-to-list 'load-path "~/.emacs.d/vendor/svg-line")

(if window-system
    (require 'ocodo-slim-svg-mode-line)
  (require 'powerline))

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;;linewrap
(global-visual-line-mode 1)


;;org-mode
(use-package org
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t))


(use-package julia-mode)

(use-package ess-site
  :ensure ess
  :defer t
  :init
  (setq ess-ask-for-ess-directory nil)
  (setq ess-eval-visibly-p nil)
  :commands R
  :commands julia)

;;auto-complete
(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;;yasnippet
;; start yasnippet with emacs
(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda()
			      (setq yas-dont-activate t))))

;;aspell and path
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-dictionary "en_GB")

;;iedit amazeballs hack
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; parenthesis mode
(show-paren-mode 1)

;;god-mode
(use-package god-mode
  :init
  (global-set-key (kbd "<escape>") 'god-local-mode))

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;;flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; smooth scroll
(use-package smooth-scrolling)

;;linum test:

(global-linum-mode t)
(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
  (setq linum-format 'linum-format-func))

;;tabs for c++
(setq c-default-style "linux"
      c-basic-offset 4)

;;auto brackets
(use-package autopair
  :init
  (autopair-global-mode 1)
  (setq autopair-autowrap t))

(setq ring-bell-function #'ignore)

;;mactex location
(getenv "PATH")
(setenv "PATH"
	(concat
	 "/usr/texbin" ":"

	 (getenv "PATH")))

;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook (lambda () (helm-mode -1)))
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
(require 'server)
(unless (server-running-p)
  (server-start))

;; Use Skim as viewer, enable source <-> PDF sync
(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("latexmk" "latexmk -pdflatex='pdflatex --shell-escape --file-line-error -synctex=1' -pdf -output-directory=Output %s" TeX-run-TeX nil t
				:help "Run latexmk on file")
			      TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("xelatex" "xelatex --shell-escape --file-line-error --synctex=1 -output-directory=Output %s && ln -s Output/*.pdf ." TeX-run-command nil t
				:help "Run xelatex on file, need Output directory")
			      TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("Clean" "TeX-clean ./Output/%s" TeX-run-command nil t
				:help "Run bibtex in current directory")
			      TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("BibTeX" "bibtex ./Output/%s" TeX-run-command nil t
				:help "Run bibtex in current directory")
			      TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("pdflatex" "pdflatex --file-line-error --synctex=1 -output-directory=Output --shell-escape %s && ln -s Output/*.pdf ." TeX-run-TeX nil t
				:help "Run pdflatex on file, need output directory")
			      TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("pdflatex_noop" "pdflatex --file-line-error --synctex=1 --shell-escape %s" TeX-run-TeX nil t
				:help "Run pdflatex on file, no output directory")
			      TeX-command-list)))

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "pdflatex")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; ========== Place Backup Files in Specific Directory ==========


;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 3               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 3               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      )

(setq auto-save-default nil)
(put 'upcase-region 'disabled nil)
