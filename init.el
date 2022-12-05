; Initialize package sources
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

(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setenv "BASH_ENV" "~/.bashrc")

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

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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

;;Magit
(use-package magit
  :commands (magit)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; LATEX MODE SETUP
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

(defmacro call-with-negative-argument (command)
  `(lambda ()
     (interactive)
     (,command -1)))

(with-eval-after-load "latex"
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

;; Use pdf-tools to open PDF files
(use-package pdf-tools)
(pdf-loader-install) ; On demand loading, leads to faster startup time

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

;;Ispell
(use-package flycheck-aspell
  :commands (ispell-buffer))

(setq ispell-program-name "C:\\msys64\\mingw64\\bin\\aspell.exe")
(setq ispell-personal-dictionary "~/.emacs.d/.ispell")
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
  (setq org-log-done t))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package conda
  :ensure t)
(require 'conda)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "~/Anaconda3")
 '(custom-safe-themes
   '("ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" default))
 '(org-agenda-files
   '("~/Desktop/Documentation.org" "u:/Travaux/Simulations/Simulations.org" "u:/Travaux/Reunions/Amplitude/RetD/planning.org" "u:/Travaux/Reunions/Amplitude/Sprint/ENFSBS/sprint_novembre.org" "c:/Users/rht/agenda.org" "u:/Travaux/Suivi_manipulations/HERA/HERA.org" "u:/Travaux/Présentations/Points tripartite/point_tripartite_novembre.org" "//serveur-prod/utilisateurs/rht/Travaux/Simulations/Developpement/Laser_tools/Lasertool.org" "u:/Travaux/Présentations/Présentations.org" "u:/Travaux/ENFSBS_suivi_projet.org" "c:/Users/rht/Desktop/Contact.org" "u:/Travaux/Suivi_manipulations/Seeder_Aerodiode/Mesures_perf.org" "u:/Travaux/Suivi_manipulations/Cellule_V1/Experiments_cell_V1.org" "u:/Travaux/Suivi_manipulations/CR_RGA_YAG/Source_laser_ENFSBS.org" "u:/Travaux/to_do_list_divers.org"))
 '(package-selected-packages
   '(dashboard py-autopep8 blacken elpy pyenv flycheck-grammalecte flyspell-correct-ivy flyspell-correct flycheck-aspell visual-fill-column org-bullets counsel-projectile projectile taxy-magit-section pdf-tools auctex magit ivy command-log-mode doom-modeline use-package conda)))

(use-package elpy
  :hook (python-mode)
  :ensure t
  :init
  (elpy-enable))

(use-package py-autopep8
  :config
  (setq py-autopep8-options '("--max-line-length=79" "--aggressive"))
  :hook ((python-mode) . py-autopep8-mode)
  )

(use-package flycheck
  :hook python-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq flycheck-flake8-maximum-line-length 99)
(setq flycheck-python-pylint-executable "~/Anaconda3/Scripts/pylint")

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
