(setq my/python-data-path (expand-file-name "~/Anaconda3/envs/data/python.exe"))
(setq my/grammalecte-script-path (expand-file-name "~/.emacs.d/grammalecte/grammalecte_flycheck.py"))
(setq my/shell-file-name "c:/Program Files/Git/git-bash")
(setq my/bash-profile-path (expand-file-name "~/.bashrc"))
(setq my/startup-banner (expand-file-name "~/.emacs.d/logo/logo_amplitude_2.png"))
(setq my/recentf-path (expand-file-name "~/.emacs.d/recentf"))
(setq my/bookmarks-path (expand-file-name "~/.emacs.d/bookmarks"))
(setq my/magit-git-executable "C:\\Program Files\\Git\\mingw64\\bin\\git.exe")
(setq my/latex-template-path "u:/Autres/Template/Document_latex/")
(setq my/hunspell-path "C:/msys64/mingw64/bin/hunspell.exe")
(setq temporary-file-directory "~/.temp/")
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq confirm-kill-emacs #'y-or-n-p)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun insert-caps-accentuated-e ()
    "Insert capital é at point"
    (interactive)
    (insert "É"))

  (global-set-key (kbd "M-é") 'insert-caps-accentuated-e)

(define-key global-map "\M-n" 'forward-paragraph)
(define-key global-map "\M-p" 'backward-paragraph)

(global-auto-revert-mode t)

(let ((dir ".emacs-backups"))
  (setq auto-save-file-name-transforms `(("\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat dir "/\\2")))
        backup-directory-alist `((".*" . ,dir))))

(setq w32-use-native-file-dialog t)

(setq tramp-mode nil)

(setq explicit-shell-file-name my/shell-file-name)
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setenv "BASH_ENV" my/bash-profile-path)

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq inhibit-compacting-font-caches t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; (with-eval-after-load 'font-lock
;;   (unless (facep 'font-lock-operator-face)
;;     (copy-face 'font-lock-keyword-face 'font-lock-operator-face)))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode 1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(global-hl-line-mode +1)    ;Highlight current line
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                pdf-view-mode-hook
                doc-view-mode-hook
                image-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(defface my-strong-bold-face
  '((t (:weight extra-bold :foreground "white")))
  "test doc")

(defun bold-first-two-letters-buffer ()
  "Met en évidence les deux premières lettres de chaque mot."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\b\\w\\{2\\}" nil t)
      (add-text-properties
       (match-beginning 0)
       (match-end 0)
       '(face my-strong-bold-face)))))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package all-the-icons)

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package focus
  :commands focus-mode)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-icon-type 'all-the-icons) 
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  )

(global-set-key (kbd "C-& C-d") 'dashboard-open)

(with-eval-after-load "dashboard"
  (setq dashboard-banner-logo-title "Time to work")
  (setq dashboard-startup-banner my/startup-banner)
  (setq dashboard-footer-messages
        '("En mode loque"
          "Maitrise de la suite office (⌐□_□)"
          "While any text editor can save your files, only Emacs can save your soul"
          "You can download our code from the URL supplied. Good luck downloading the only postdoc who can get it to run, though"
          "Les barreaux lasers c'est comme les cyclistes, plus c'est dopé, plus il y a de gain."
          "When I see a bird that walks like a duck and swims like a duck and quacks like a duck, I call that bird a duck"
          "Gaussian beam waist = lambda f / pi d"
          "Top-hat beam waist = 1.22 lambda f / d"
          )))

(run-at-time (current-time) 300 'recentf-save-list)
(add-to-list 'recentf-exclude my/recentf-path)
(add-to-list 'recentf-exclude my/bookmarks-path)
(with-eval-after-load 'org
  (when (boundp 'org-agenda-files)
    (dolist (f org-agenda-files)
      (add-to-list 'recentf-exclude f))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1)
  :custom
  (setq all-the-icons-ivy-rich-icon t)
  (setq all-the-icons-ivy-rich-color-icon t))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :config
  (global-company-mode 1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package yasnippet
  :hook (LaTeX-mode . yas-minor-mode)
  (python-mode . yas-minor-mode)
  :config
  (setq my/yas-snippet-dirs  (expand-file-name "snippets" user-emacs-directory))
)

(use-package flycheck
:config
(global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit))

(with-eval-after-load 'flycheck
    (setq flycheck-flake8-maximum-line-length 79))

(defun my/grammalecte-command (source)
  (list my/python-data-path my/grammalecte-script-path source))

(with-eval-after-load 'flycheck
  (flycheck-define-checker grammalecte
    "A French grammar checker using Grammalecte."
    :command ("python"
            (eval my/grammalecte-script-path)
            "-")
    :standard-input t
    :error-patterns
    (
     (warning line-start "<stdin>:" line ":" column "-" end-column ": warning: " (message) line-end)
     (error   line-start "<stdin>:" line ":" column "-" end-column ": error: " (message) line-end)
     )
    :modes (text-mode markdown-mode LaTeX-mode org-mode))
  (add-to-list 'flycheck-checkers 'grammalecte)
  (flycheck-add-next-checker 'grammalecte 'tex-chktex))

(use-package flyspell
  :after flycheck
  :init
  (setq flyspell-sort-corrections nil)
  :config
  (setenv "LANG" "fr")
  (setq ispell-program-name my/hunspell-path)
  (setq ispell-dictionary "fr")
  (setq ispell-personal-dictionary
        (expand-file-name ".ispell" user-emacs-directory))
  (setq flyspell-delay 1)
  )

(use-package flyspell-correct
  :ensure  t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-at-point))
  )

(use-package flyspell-correct-ivy
  :ensure t
  :demand t
  :after flyspell-correct
  )

(defun my/org-mode-setup ()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (text-scale-increase 2)
  (setq org-ellipsis "▼")
  (setq org-columns-ellipses "▼")
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

(defun my/org-create-latex-snippet ()
  (interactive)
  (insert "$$$$")
  (backward-char 2)
  (org-edit-special))

(defun my/org-mode-editing-setup ()
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("la" . "src latex"))
  (add-to-list 'org-src-lang-modes '("latex" . latex))
  (setq org-latex-create-formula-image-program 'dvipng)
  (setq org-preview-latex-default-process 'dvipng)
  (setq org-startup-with-latex-preview t)
  (setq org-fromat-latex-options
        (plist-put org-format-latex-options :scale 2.0))
  (setq org-src-window-setup 'current-window)    
  (setq electric-pair-inhibit-predicate
        (lambda (c)
          (if (char-equal c ?\<) t (electric-pair-default-inhibit c))))
  )

(defun my/org-mode-agenda-setup ()

  (setq org-agenda-hide-tags-regexp "Scheduled")
  (setq org-agenda-scheduled-leaders '("" ""))
  (setq org-agenda-include-diary t)
  (setq calendar-dat-style 'european)
  (setq diary-show-holidays-flag nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "PLAN(p)" "ACTIVE(a)" "|" "Done(d)")))

  (setq org-log-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-log-done 'time)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-log-mode-items '(closed))
  (setq org-tag-alist
        '(("@Me" . ?r)
          ("@BEE" . ?e)
          ("@BEM" . ?m)
          ("@Indus" . ?i)
          ("@RGP" . ?g)
          ("@Achats" . ?a)
          ("@Direction" . ?d))
        )

  (setq org-agenda-tags-column -100)
  (setq org-agenda-align-tags t)

  (temp-buffer-resize-mode 1)
  (setq temp-buffer-max-height 30)

  (setq org-archive-location ".emacs-backups/archives.org::")
    )

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree)
  )

(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (
         (org-mode . my/org-mode-setup)
         (org-mode . my/org-mode-agenda-setup)
         (org-mode . my/org-mode-editing-setup))
  :config
  (define-key global-map "\C-cc" 'org-archive-done-tasks)
  (define-key org-mode-map (kbd "C-c l") #'my/org-create-latex-snippet)
  (require 'org-tempo)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'my/org-babel-tangle-config)))
  )

(define-key global-map "\C-ca" 'org-agenda)

(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-git-executable my/magit-git-executable))

(defun my/latex-config ()
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil) 
  (setq-default TeX-master t))

(defun my/latex-setup ()
  (visual-line-mode)
  (LaTeX-math-mode)
  (flyspell-mode))

(use-package pdf-tools
      :init
      (setq pdf-cache-prefetch-delay -1)
      :config
      (pdf-tools-install))

(defun my/latex-compilation-parameters ()
(setq TeX-PDF-mode t
      TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          (lambda (_process) (TeX-view))
          nil t))

(defun my/latex-editing-parameters ()
  (add-to-list 'LaTeX-math-list '("M-p" "partial" "" 2202))
  (add-to-list 'LaTeX-math-list '("M-r" "overrightarrow" "" nil))
  (add-to-list 'LaTeX-math-list '("M-:" "frac" "" nil))
  (add-to-list 'LaTeX-math-list '("M-c" "text{c.c.}" "" nil))
  (add-to-list 'LaTeX-math-list '("M-." "dot" "" 15))
  (add-to-list 'LaTeX-math-list '(" " "&" "" nil))
  (setq LaTeX-math-abbrev-prefix "&")
  (setq TeX-electric-sub-and-superscript t)
  (setq LaTeX-electric-left-right-brace t)
  (setq TeX-electric-math '("$" . "$")))

(defun my/latex-flycheck-parameters ()
  (setq flycheck-chktexrc "~/.chktexrc")
  (setq reftex-plug-into-AUCTeX t))

(defun my/latex-disable-company-capf ()
    "Remove company-capf backend in LaTeX buffers."
    (setq-local company-backends
                (remove 'company-capf company-backends)))

(defvar my/si-tokens
  '((""  . "")
    ("Yotta" . "\\yotta")
    ("Zetta" . "\\zetta")
    ("Exa" . "\\exa")
    ("Peta" . "\\peta")
    ("Tera" . "\\tera")
    ("Giga" . "\\giga")
    ("Mega" . "\\mega")
    ("kilo" . "\\kilo")
    ("hecto" . "\\hecto")
    ("deca" . "\\deca")
    ("deci" . "\\deci")
    ("centi" . "\\centi")
    ("milli" . "\\milli")
    ("µ micro" . "\\micro")
    ("nano" . "\\nano")
    ("pico" . "\\pico")
    ("femto" . "\\femto")
    ("atto" . "\\atto")
    ("zepto" . "\\zepto")
    ("yocto" . "\\yocto")
    ("per" . "\\per")
    ("meter"   . "\\meter")
    ("gram"   . "\\gram")
    ("second"   . "\\second")
    ("Ampere"   . "\\ampere")
    ("Kelvin"   . "\\kelvin")
    ("mole" . "\\mole")
    ("Candela"  . "\\candela")
    ("Hertz"  . "\\hertz")
    ("Newton"   . "\\newton")
    ("Pascal"  . "\\pascal")
    ("Joule"   . "\\joule")
    ("Watt"   . "\\watt")
    ("Coulomb"   . "\\coulomb")
    ("Volt"   . "\\volt")
    ("Farad"   . "\\farad")
    ("Ω ohm"   . "\\ohm")
    ("Siemens"   . "\\siemens")
    ("Weber"  . "\\weber")
    ("Telsa"   . "\\tesla")
    ("Henry"   . "\\henry")
    ("lumen"  . "\\lumen")
    ("lux"  . "\\lux")
    ("Becquerel"  . "\\becquerel")
    ("Gray"  . "\\gray")
    ("Sievert"  . "\\sievert")
    ("katal" . "\\katal")
    ("² squared" . "^2")
    ("inv" . "^{-1}")))

(defun my/latex-insert-SI ()
  (interactive)
  (let ((value (read-string "Value: "))
        key token tokens)

    (while
        (not
         (string-empty-p
          (setq key
                (completing-read
                 "Token (RET to finish): "
                 (mapcar #'car my/si-tokens)
                 nil nil nil nil ""))))

      (setq token (cdr (assoc key my/si-tokens)))
      (when token
        (push token tokens)))

    (insert
     (format "\\SI{%s}{%s}"
             value
             (mapconcat #'identity (reverse tokens) "")))))

(defun Latex-include-graphics (width filename)
  "Prompts for figure width and figure path and include image at path with width = width * linewidth"
  (interactive "sWidth: \nfInsert file name: ")
  (insert "\\includegraphics[width="width"\\linewidth]{"(file-relative-name filename)"}"))

(use-package eglot
  :after latex
  :hook (
         (LaTeX-mode . (lambda ()
                         (setq eglot-server-programs `((latex-mode . ("texlab"))))
                         (eglot-ensure))))
   :config
   (setq eglot-connect-timeout 10)
   (setq eglot-autoshutdown t)
   (define-key LaTeX-mode-map (kbd "C-c l r r") #'eglot-rename)
   (define-key LaTeX-mode-map (kbd "C-c l = =") #'eglot-format-buffer)
   )

(defun Create-new-LaTeX-document (destination)
  "Copie récursivement le contenu d'un dossier source fixe vers DESTINATION."
  (interactive
   (list (read-directory-name "Coller le contenu du dossier fixe ici : ")))
  (let ((source my/latex-template-path))
    (unless (file-directory-p source)
      (error "Le dossier source n'existe pas ou n'est pas un répertoire"))
    (unless (file-directory-p destination)
      (make-directory destination t))
    (dolist (file (directory-files source t "^[^.]")) ; ignore . et ..
      (let ((target (expand-file-name (file-name-nondirectory file) destination)))
        (if (file-directory-p file)
            (copy-directory file target)
          (copy-file file target t))))
    (message "Copie terminée vers %s" destination)))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . my/latex-config)
         (LaTeX-mode . my/latex-setup)
         (LaTeX-mode . my/latex-compilation-parameters)
         (LaTeX-mode . my/latex-editing-parameters)
         (LaTeX-mode . my/latex-flycheck-parameters)
         (LaTeX-mode . my/latex-disable-company-capf)
         (LaTeX-mode . LaTeX-math-mode))
  :config
  (with-eval-after-load 'latex
    (define-key LaTeX-mode-map (kbd "C-c u") #'my/latex-insert-SI)
    (define-key LaTeX-mode-map (kbd "C-c i") #'Latex-include-graphics))
  )

(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")

(defun my/run-python-buffer-in-process ()
  "Exécute le buffer Python dans un processus en arrière-plan et affiche
la sortie (y compris les erreurs) dans *Messages*.  
Si le buffer n'est pas sauvegardé, demande de le sauvegarder avant l'exécution."
  (interactive)
  (when (or (not (buffer-file-name))
            (buffer-modified-p))
    (if (y-or-n-p "Le buffer n'est pas sauvegardé. Sauvegarder maintenant ? ")
        (save-buffer)
      (error "Le buffer doit être sauvegardé pour exécuter le script")))
  (let ((file (buffer-file-name)))
    (let ((output-buffer (get-buffer-create "*python-output-temp*")))
      (with-current-buffer output-buffer
        (erase-buffer))
      (let ((proc (start-process "python-process"
                                 output-buffer
                                 "python"
                                 "-u"
                                 file)))
        (set-process-filter
         proc
         (lambda (process string)
           (with-current-buffer (process-buffer process)
             (goto-char (point-max))
             (insert string))))
        (set-process-sentinel
         proc
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (let ((output (with-current-buffer (process-buffer process)
                             (buffer-string))))
               (message "%s" output)))))))))

(defun my/debug-python-buffer ()
  "Lance pdb sur le buffer Python courant dans l'autre fenêtre.
Si l'écran est déjà split, utilise l'autre fenêtre. Sinon, crée un split horizontal.
Le point est placé dans le buffer debugger."
  (interactive)
  (unless (buffer-file-name)
    (error "Le buffer doit être sauvegardé sur disque avant de lancer pdb"))
  (when (buffer-modified-p)
    (if (y-or-n-p "Buffer modifié. Sauvegarder ? ")
        (save-buffer)
      (error "Le buffer doit être sauvegardé pour lancer pdb")))
  (let* ((orig-file (buffer-file-name))
         (cur-window (selected-window))
         (other-win (if (> (length (window-list)) 1)
                        (seq-find (lambda (w) (not (eq w cur-window)))
                                  (window-list))
                      (split-window-right)))
         dbg-buffer)
    (select-window other-win)
    (setq dbg-buffer (realgud:pdb (concat "python -m pdb \"" orig-file "\"")))
    (switch-to-buffer dbg-buffer)
    (select-window other-win)))

(defun my/realgud-kill ()
  "Supprime le processus associé au buffer realgud courant lors de la fermeture du buffer."
  (add-hook 'kill-buffer-hook
            (lambda ()
              (let ((proc (get-buffer-process (current-buffer))))
                (when (and proc (process-live-p proc))
                  (delete-process proc))))
            nil t))

(use-package realgud
  :defer t
  :commands (realgud:pdb)
  :hook (realgud:track-mode . my/realgud-kill)
  :config
  (setq realgud:shortkey-mode nil)
  (setq realgud:frame-arrow-type 'left)
  )

(defun my/python-init-script (doc)
  "Insert the script documentation, import typical packages and writes the main fun"
  (interactive "sDocumentation: ")
  (insert "\"\"\" "doc"
Author: Raphaël Humblot
Date:")
    (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
    (insert "\"\"\"
from pathlib import Path
import numpy as np
import numpy.typing as npt
import matplotlib.pyplot as plt
from matplotlib.axes import Axes
from matplotlib.figure import Figure


def main():
    \"\"\""doc"\"\"\"


if __name__ == \"__main__\":
    main()
"
        ))

(defun my/python-insert-path-to-file (filename)
  "Insert a relative path from current python file to target file. Useful for data import"
  (interactive "fFile: ")
  (insert "Path(__file__).parent " "/ " (mapconcat
                                          (lambda (s) (format "\"%s\"" s))
                                          (split-string (file-relative-name filename) "/")
                                          " / ")
          ))

(defun my/python-insert-figure-save (name)
  "Insert fig.savefig command"
  (interactive "sFigure name: ")
  (insert "fig.savefig(Path(__file__).parent / \""name".png\", dpi=400, bbox_inches=\"tight\")"))

(defun my/black-format-current-buffer ()
  "Format current buffer with black."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "black -q -l 79 \"" buffer-file-name "\""))
    (revert-buffer t t )))

(defun my/python-set-keymaps ()
  (define-key python-mode-map (kbd "C-c M-m") 'my/python-init-script)
  (define-key python-mode-map (kbd "C-c f") 'my/python-insert-path-to-file)
  (define-key python-mode-map (kbd "C-c M-f") 'my/python-insert-figure-save)
  (define-key python-mode-map (kbd "C-<backspace>") 'backward-kill-word)
  (define-key python-mode-map (kbd "C-c l b") #'my/black-format-current-buffer)
  (define-key python-mode-map (kbd "C-c C-c") #'my/run-python-buffer-in-process)
  (define-key python-mode-map (kbd "C-c d") #'my/debug-python-buffer)
  )

(defun my/lsp-setup ()
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-multi-root t)
  (setq lsp-allow-server-download nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-log-io nil)
  (setq lsp-client-packages '(lsp-pyright))
  (setq lsp-pyright-multi-root nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-pyright-python-executable-cmd "python")
  (setq lsp-enable-which-key-integration 1)
  )

(defun my/lsp-session-localisation ()
  (setq lsp-session-file (expand-file-name "lsp-session" user-emacs-directory))
  )

(use-package lsp-mode
  :hook ((python-mode . lsp-deferred))
  :commands lsp
  :init
  (my/lsp-setup)
  (my/lsp-session-localisation)
  )

(use-package lsp-pyright
  :after lsp-mode
  :config
  (setq lsp-disabled-clients '(pyls))
  )

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(defun my/python-setup ()
(electric-pair-mode 1))

(use-package python
  :hook ((python-mode . my/python-set-keymaps)
          (python-mode . my/python-setup))
  )

(defun my/projectile-setup ()
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-files
        (append '(".aux"
                  "bbl"
                  "blg"
                  "~")
                projectile-globally-ignored-files))
  (setq projectile-globally-ignored-directories (append '("*.emacs-backups*")
                projectile-globally-ignored-directories))
  (if (file-directory-p "u:/Travaux")
      (setq projectile-project-search-path
            (append '("u:/Travaux/Projets") projectile-project-search-path)))
  )

(use-package projectile
:diminish projectile-mode
:commands (projectile-find-file projectile-switch-project)
:bind-keymap ("C-c p" . projectile-command-map)
:hook (projectile-mode . my/projectile-setup)
:config (projectile-mode)
(message "Projectile loaded !"))

(use-package counsel-projectile
  :hook projectile-mode-hook
  :config (counsel-projectile-mode))

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


               (defun suppress-messages (func &rest args)
                 "Suppress message output from FUNC."
                 ;; Some packages are too noisy.
                 ;; https://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
                 (cl-flet ((silence (&rest args1) (ignore)))
                   (advice-add 'message :around #'silence)
                   (unwind-protect
                       (apply func args)
                     (advice-remove 'message #'silence))))

               ;; Suppress "Cleaning up the recentf...done (0 removed)"
               (advice-add 'recentf-cleanup :around #'suppress-messages)(insert " : "))))

  (defun add-add-dependency ()
    (local-set-key (kbd "C-c C-d") 'add-dependency))

(add-hook 'makefile-mode-hook #'add-add-dependency)

(message "Emacs init buffer evaluated to the end !")
