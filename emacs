;;melpa packagemanager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(setq explicit-shell-file-name "/usr/local/bin/zsh")

(setq multi-term-program "/bin/zsh")

(setq inhibit-startup-screen t)

;; get correct terminal
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))

;test new mac git

;;helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)


;; indent
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

					;rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; powerline
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
;;(require 'powerline)
;;(setq powerline-arrow-shape 'curve)

;; theme
(if window-system
    (setq solarized-high-contrast-mode-line t)) ;;required for svg modeline.

;;theme
(if window-system
    (load-theme 'solarized-dark t)
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


(set-face-attribute 'default nil :foundry "apple" :family "Source Code Pro for Powerline" :height 140)


;;org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

					;ess-mode configuration
(setq ess-ask-for-ess-directory nil) 
(setq inferior-R-program-name "/usr/local/bin/R") 
(setq ess-local-process-name "R") 
(setq ansi-color-for-comint-mode 'filter) 
(setq comint-scroll-to-bottom-on-input t) 
(setq comint-scroll-to-bottom-on-output t) 
(setq comint-move-point-for-output t)
(setq ess-eval-visibly-p nil)
(require 'ess-site)


;;auto-complete
(require 'auto-complete)
					; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;;yasnippet
					; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;;aspell and path
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-dictionary "en_GB")

;;iedit amazeballs hack
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; parenthesis mode
(show-paren-mode 1)

;;god-mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

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
(require 'smooth-scrolling)

;;matlab-emacs
(add-to-list 'load-path "~/.emacs.d/personal/matlab-emacs")
(load-library "matlab-load")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex --synctex=1 -output-directory=Output --shell-escape")
 '(TeX-shell "/usr/local/bin/zsh")
 '(TeX-view-program-selection (quote ((output-pdf "PDF Viewer"))) t)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" default)))
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
 '(mlint-programs
   (quote
    ("mlint" "mac/mlint" "/Applications/MATLAB_R2014b.app/bin/maci64/mlint")))
 '(ns-alternate-modifier (quote meta))
 '(nxml-slash-auto-complete-flag t)
 '(preview-gs-options
   (quote
    ("-q" "-dDELAYSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4" "-dNOSAFER"))))
(add-hook 'matlab-mode-hook 'auto-complete-mode)
(setq auto-mode-alist
      (cons
       '("\\.m$" . matlab-mode)
       auto-mode-alist))



;linum test:

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
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)


(setq ring-bell-function #'ignore)

;;flyspell for latex
					;(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

(add-hook 'LaTeX-mode-hook 
	  (lambda () 
	    (TeX-fold-mode 1)
	    (add-hook 'find-file-hook 'TeX-fold-buffer t t)
	    (add-hook 'after-change-functions 
		      (lambda (start end oldlen) 
			(when (= (- end start) 1)
			  (let ((char-point 
                                 (buffer-substring-no-properties 
                                  start end)))
			    (when (or (string= char-point "}")
				      (string= char-point "$"))
			      (TeX-fold-paragraph)))))
		      t t)))



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
			      '("latexmk" "latexmk -pdflatex='pdflatex --shell-escape -synctex=1' -pdf -output-directory=Output %s" TeX-run-TeX nil t
				:help "Run latexmk on file")
			      TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("xelatex" "xelatex --shell-escape --synctex=1 -output-directory=Output %s && ln -s Output/*.pdf ." TeX-run-command nil t
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
			      '("pdflatex" "pdflatex --synctex=1 -output-directory=Output --shell-escape %s && ln -s Output/*.pdf ." TeX-run-TeX nil t
				:help "Run pdflatex on file, need output directory")
			      TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("pdflatex_noop" "pdflatex --synctex=1 --shell-escape %s" TeX-run-TeX nil t
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

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

(setq auto-save-default nil)




(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
