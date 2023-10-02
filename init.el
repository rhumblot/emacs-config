;;; Initialize package sources

;;; Code:
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))
(package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; windows compatibility
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq explicit-shell-file-name "c:/Program Files/Git/git-bash")
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setenv "BASH_ENV" "~/.bashrc")

;; auto-saves in .emacs-backups folder
(let ((dir ".emacs-backups"))
  (setq auto-save-file-name-transforms `(("\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat dir "/\\2")))
        backup-directory-alist `((".*" . ,dir))))

;; Appearance customization
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(global-hl-line-mode +1)    ;Highlight current line
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(column-number-mode)
(global-display-line-numbers-mode t)

(use-package dashboard
:ensure t
:config
(dashboard-setup-startup-hook))
;;Set the title
(setq dashboard-banner-logo-title "Time to work")
(setq dashboard-startup-banner "~/.emacs.d/logo/logo_amplitude_2.png")
;;Set the banner

(setq dashboard-week-agenda t)
(setq dashboard-icon-type 'all-the-icons) ;;
(setq dashboard-set-file-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")
(setq dashboard-set-init-info t)

(setq dashboard-footer-messages
'("En mode loque"
  "Maitrise de la suite office (⌐□_□)"
  "While any text editor can save your files, only Emacs can save your soul"
  "You can download our code from the URL supplied. Good luck downloading the only postdoc who can get it to run, though"
  "Les barreaux lasers c'est comme les cyclistes, plus c'est dopé, plus il y a de gain."
  "When I see a bird that walks like a duck and swims like a duck and quacks like a duck, I call that bird a duck"
  "Gaussian beam waist = lambda f / pi d"
  "Top-hat beam waist = 1.22 lambda f / d"
  ))

;; Save recentf at regular intervals
(run-at-time (current-time) 300 'recentf-save-list)
;; Exclude the recentf file itself
(add-to-list 'recentf-exclude
         (expand-file-name "~/.emacs.d/recentf")
	     );;;
(add-to-list 'recentf-exclude
	     (expand-file-name "~/.emacs.d/bookmarks"))

(use-package focus
  :commands focus-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		image-mode
		pdf-view-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Encodage en UTF-8
(setq inhibit-compacting-font-caches t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package all-the-icons)
(unless (package-installed-p 'doom-themes)
  (all-the-icons-install-fonts))

(use-package doom-themes
  :init (load-theme 'doom-one t))

;; Set up the visible bell
(setq visible-bell t)

;;Add escape as an escape key
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Add shortcut for accentuated é
(global-set-key (kbd "M-é") 'insert-caps-accentuated-e)
(global-set-key (kbd "C-& C-d") 'dashboard-open)
;; Autocompletion and finding files
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(ivy-mode 1)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

;; Whether display the icons
(setq all-the-icons-ivy-rich-icon t)

;; Whether display the colorful icons.
;; It respects `all-the-icons-color-icons'.
(setq all-the-icons-ivy-rich-color-icon t)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package company
  :config
  (global-company-mode 1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package flycheck
  :hook python-mode)


(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq flycheck-flake8-maximum-line-length 99)
;;(setq flycheck-python-pylint-executable "~/Anaconda3/Scripts/pylint")

;; (use-package flycheck-grammalecte
;;   :after flycheck
;;   :config
;;   (flycheck-grammalecte-setup))
;;  (setq flycheck-grammalecte-report-esp nil)
;; (setq grammalecte-python-package-directory "c:/Users/rht/Anaconda3/envs/grammalecte")
;; (require 'flycheck-grammalecte)
;;  (add-to-list 'flycheck-grammalecte-enabled-modes 'fountain-mode)
;;  (grammalecte-download-grammalecte)
;;  (flycheck-grammalecte-setup))

;;Magit
(use-package magit
  :commands (magit)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq magit-git-executable '"C:\\Program Files\\Git\\mingw64\\bin\\git.exe")

;; LATEX MODE SETUP

(defmacro call-with-negative-argument (command)
  `(lambda ()
     (interactive)
     (,command -1)))

(with-eval-after-load "latex"
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  '(LaTeX-math-abbrev-prefix "&")
  '(LaTeX-math-list '(("M-p" "partial" "" 2202)))
  '(TeX-electric-sub-and-superscript t)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (define-key LaTeX-mode-map (kbd "C-c u")
    (defun Latex-insert-unit (value unit)
      "Prompts for value and unit and insert the latex command that corresponds to this value"
      (interactive "sValue: \nsUnit: \n")
      (insert "$\\unit{" value "}{" unit "}$ ")))
    (define-key LaTeX-mode-map (kbd "C-c i")
      (defun Latex-include-graphics (width filename)
	"Prompts for figure width and figure path and include image at path with width = width * linewidth"
	(interactive "sWidth: \nfInsert file name: ")
	(insert "\\includegraphics[width="width"\\linewidth]{"(file-relative-name filename)"}"))))

(defun insert-caps-accentuated-e ()
  "Insert capital é at point"
  (interactive)
  (insert "É"))

;; Use pdf-tools to open PDF files
(use-package pdf-tools
  :defer)
(pdf-loader-install) ; On demand loading, leads to faster startup time


;;Ispell
(use-package flyspell
  :after flycheck
  :init
  (setq flyspell-sort-corrections nil)
  )

(setq ispell-program-name "c:/Program Files (x86)/Hunspell/bin/hunspell.exe")
(setq ispell-personal-dictionary "~/.emacs.d/.ispell")
(setq ispell-local-dictionary-alist '(
				      (nil
				       "[[:alpha:]]"
				       "[^[:alpha:]]"
				       "[']"
				       t
				       ("-d" "en_US" "-p" "D:\\hunspell\\share\\hunspell\\personal.en")
				       nil
				       iso-8859-1)
				      ("francais"
				       "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
				       "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
				       "[-']"
				       t
				       ("-d" "fr" "-p" 
					"D:\\hunspell\\share\\hunspell\\personal.fr")
				       nil
				       utf-8)
				      ))
(require 'ispell)
 
(use-package flyspell-correct
  :ensure  t
  :commands (ispell-buffer)
  :bind (:map flyspell-mode-map
          ("C-;" . flyspell-correct-at-point))
  )

(use-package flyspell-correct-ivy
  :ensure t
  :demand t
  :after flyspell-correct
  )

;; ORGMODE SETUP
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------
 (defun scale-up-font ()
   (text-scale-increase 2))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook ((org-mode . efs/org-mode-setup)
	 (org-mode . scale-up-font))
  :config
  (setq org-ellipsis " ▼")
  (setq org-agenda-hide-tags-regexp "Scheduled")
  (setq org-agenda-scheduled-leaders '("" ""))
  (setq org-todo-keywords
    '((sequence "TODO(t)" "|" "DONE(d)")
      (sequence "PLAN(p)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (setq org-log-done t)
  (setq org-archive-location ".emacs-backups/archives.org::")
  (dolist (files org-agenda-files)
    (add-to-list 'recentf-exclude files)
    ))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\M-n" 'forward-paragraph)
(define-key global-map "\M-p" 'backward-paragraph)
(define-key global-map "\C-cc" 'org-archive-done-tasks)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix "&")
 '(LaTeX-math-list '(("M-p" "partial" "" 2202)))
 '(TeX-electric-sub-and-superscript t)
 '(conda-anaconda-home "~/Anaconda3")
 '(custom-safe-themes
   '("ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" default))
 '(ispell-local-dictionary "fr")
 '(org-agenda-files
   '("u:/Travaux/Formations/formations.org" "u:/Travaux/anniversaires.org" "u:/Travaux/Suivi_manipulations/Cellule_100Hz/SBS_100Hz.org" "u:/Travaux/Publications/SBS_high_energy/Publication_SBS_high_energy.org" "u:/Travaux/Autres/Design_Apollon/report_Apollon.org" "u:/Travaux/Presentations/Siegman_School/poster.org" "u:/Travaux/Simulations/Simulations.org" "u:/Travaux/Reunions/reunion.org" "c:/Users/rht/Desktop/Documentation.org" "u:/Travaux/Reunions/Amplitude/RetD/planning.org" "u:/Travaux/Suivi_manipulations/HERA/HERA.org" "u:/Travaux/Suivi_manipulations/Cellule_V1/Experiments_cell_V1.org" "u:/Travaux/Suivi_manipulations/CR_RGA_YAG/Source_laser_ENFSBS.org" "u:/Travaux/to_do_list_divers.org"))
 '(package-selected-packages
   '(flycheck-grammalecte all-the-icons-ivy-rich all-the-icons-ivy page-break-lines elpy company-prescient ivy-prescient py-autopep8 blacken pyenv flyspell-correct-ivy flyspell-correct flycheck-aspell visual-fill-column org-bullets counsel-projectile projectile taxy-magit-section pdf-tools auctex magit ivy command-log-mode doom-modeline use-package conda))
 '(warning-suppress-log-types '((comp) (comp) (comp) (auto-save)))
 '(warning-suppress-types '((comp) (comp) (auto-save))))

(use-package elpy
  :hook (python-mode)
  :ensure t
  :init
  (elpy-enable))

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))

;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "python"))
		)))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python")
		)))

;; (use-package py-autopep8
;;   :config
;;   (setq py-autopep8-options '("--max-line-length=79" "--aggressive"))
;;   :hook ((python-mode) . py-autopep8-mode)
;;   )

(use-package flycheck
  :hook python-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq flycheck-flake8-maximum-line-length 99)
(setq flycheck-python-pylint-executable "~/Anaconda3/Scripts/pylint")
(setq elpy-rpc-python-command "~/Anaconda3/envs/elpy/pythonw.exe")
;;Projectile
;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map))
;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

(defun my-insert-file-name (filename &optional args)
    "Insert name of file FILENAME into buffer after point.
  
  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.
  
  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.
  
  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
    ;; Based on insert-file in Emacs -- ashawley 20080926
    (interactive "*fInsert file name: \nP")
    (cond ((eq '- args)
           (insert (file-relative-name filename)))
          ((not (null args))
           (insert (expand-file-name filename)))
          (t
           (insert filename))))
  
(global-set-key "\C-cf" 'my-insert-file-name)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun add-dependency (file bool curr-point)
  "Insert dependencies to file and insert creation command"
  (interactive
   (list
    (read-file-name "*fInsert file name: \nP")
    (y-or-n-p "Insert dependecies? ")
    (point)))
  (insert (file-relative-name file))
  (if bool (save-excursion
	     (end-of-buffer)
	     (insert "\n\n")
	     (insert (file-relative-name file))
	     (insert " : "))))

(defun add-add-dependency ()
  (local-set-key (kbd "C-c C-d") 'add-dependency))

(add-hook 'makefile-mode-hook #'add-add-dependency)
  
;;Custom set varible to switch to another .el file ASAP

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; init.el ends here
