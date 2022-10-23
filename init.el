;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Appearance customization
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

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

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


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

(use-package magit)

;; LATEX MODE SETUP

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)


;;Spellchecking
(add-to-list 'exec-path "C:/msys64/mingw64/bin")
(setq ispell-program-name "aspell")
(require 'ispell)

(add-to-list 'ispell-skip-region-alist
             '("^\\[source" . "^----\n\n") ;; source exerpts
         '("\\[\\[" . "\\]\\]")        ;; links
         )

;; ORGMODE SETUP

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-tag-alist '(("perso" . ?p) ("Computer" . ?c) ("Luli" . ?u)
		      ("IOGS" . ?i) ("Amplitude" . ?a) ("Mail" . ?m)
		      ("Bibliography" . ?u)))
(setq org-log-done t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "~/Anaconda3/envs")
 '(custom-safe-themes
   '("ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" default))
 '(inhibit-startup-screen t)
 '(org-agenda-files
   '("u:/Travaux_Raphaël/Présentations/Présentations.org" "u:/Travaux_Raphaël/ENFSBS_suivi_projet.org" "c:/Users/rht/Desktop/Contact.org" "u:/Travaux_Raphaël/Suivi_manipulations/Seeder_Aerodiode/Mesures_perf.org" "u:/Travaux_Raphaël/Suivi_manipulations/Cellule_V1/Experiments_cell_V1.org" "u:/Travaux_Raphaël/Suivi_manipulations/CR_RGA_YAG/Source_laser_ENFSBS.org" "u:/Travaux_Raphaël/Simulations/Simulations.org" "u:/Travaux_Raphaël/to_do_list_divers.org"))
 '(package-selected-packages
   '(taxy-magit-section pdf-tools auctex magit ivy command-log-mode doom-modeline use-package elpy conda)))


;; ====================================
;; Development Setup
;; ====================================

;; Enable Flycheck

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package conda)
(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
;; (conda-env-autoactivate-mode t)

(require 'python)
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (defun Latex-insert-unit (value unit)
;;   "Prompts for value and unit and insert the latex command that corresponds to this value"
;;   (interactive "sValue: \nsUnit: \n")
;;   (insert "$\\unit{" value "}{" unit "}$ "))

;; (defun Latex-include-graphics (width path)
;;   "Prompts for figure width and figure path and include image at path with width = width * linewidth"
;;   (interactive "sWidth: \nsPath: \n")
;;   (insert "\\includegraphics[width="width"\\linewidth]{"path"}"))

(with-eval-after-load "latex"
  (define-key LaTeX-mode-map (kbd "C-c u")
    (defun Latex-insert-unit (value unit)
      "Prompts for value and unit and insert the latex command that corresponds to this value"
      (interactive "sValue: \nsUnit: \n")
      (insert "$\\unit{" value "}{" unit "}$ ")))
    (define-key LaTeX-mode-map (kbd "C-c i")
      (defun Latex-include-graphics (width path)
	"Prompts for figure width and figure path and include image at path with width = width * linewidth"
	(interactive "sWidth: \nsPath: \n")
	(insert "\\includegraphics[width="width"\\linewidth]{"path"}"))))

(use-package pdf-tools)
(pdf-loader-install) ; On demand loading, leads to faster startup time
