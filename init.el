;;; Mon fichier .emacs (fichier init.el)
;; Contient mes préférences.

;; POUR NE PAS AVOIR À REDÉMARRER EMACS À CHAQUE CHANGEMENT DU .EMACS,
;; TAPER C-x C-e à la fin de chaque expression nouvelle (expression elisp
;; à évaluer).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SECTIONS :
;;;;
;;;;    LOAD PATH
;;;;    ELPA, MELPA
;;;;    LOAD PACKAGES
;;;;    KEY BINDINGS
;;;;    PARENTHESES
;;;;    SCROLLING CUSTOMIZATION
;;;;    IDO
;;;;    SPELL CHECKING
;;;;    CURSOR
;;;;    LINE NUMBERING AND HIGHLIGHTING
;;;;    GRAPHICAL USER INTERFACE
;;;;    FONTS
;;;;    POWERLINE
;;;;    THEMES
;;;;    FRAME SIZE
;;;;    BACKUP
;;;;    ORG
;;;;    AUCTEX
;;;;    SHELL
;;;;    AUTOCOMPLETE
;;;;    MERLIN
;;;;    CUSTOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LOAD PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell emacs where your personal elisp library directory is :
(add-to-list 'load-path "~/.emacs.d/lisp/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    ELPA, MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LOAD PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load the packaged named xyz :
;; (load "xyz") ;; best not to include the ending “.el” or “.elc”
(load "tuareg")
(load "ocamldebug")
(load "tuareg_indent")
(load "tuareg-site-file")

(load "frame-fns")
(load "frame-cmds")  ;; mouvements des frames

;; aggressive-indent mode
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;; enable one
(global-aggressive-indent-mode 1) ;; enable all
;; (add-to-list 'aggressive-indent-excluded-modes 'shell-script-mode)
;; disable one

;; (load "minimap") ;; M-x minimap-mode pour activer la minimap.
;; Ajouter un hook (longueur <- 108) lorsque l'on enclenche le minimap-mode.

;; Auto-start Smex every time you open Emacs.
(when (>= emacs-major-version 24)
  (load "smex")
  (require 'smex) ;; Not needed if you use package.el
  (smex-initialize) ;; Can be omitted. This might cause a (minimal) delay
  ;; when Smex is auto-initialized on its first run.
  )

;; Load lua-mode for Lua programming
;; (load "lua-mode")
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Use octave-mode for all the *.m files
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; Turn on the abbrevs, auto-fill and font-lock features automatically.
;; For more details on octave-mode setup and use, see :
;; https://www.gnu.org/software/octave/doc/interpreter/Emacs-Octave-Support.html
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    KEY BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mes keybindings persos (peuvent être changés si besoin est)

;; (global-set-key (quote [s-up]) (quote beginning-of-buffer))
;; (global-set-key (quote [s-down]) (quote end-of-buffer))

;; (add-hook 'TeX-mode-hook 'my-make-slash-backslash)

;; Special characters

(setq ns-right-alternate-modifier nil)
;; right alt is no longer a meta key !!
;; note that the following shortcuts now become virtually useless

(global-set-key (kbd "M-/") "\\")
(global-set-key (kbd "M-L") "|")
(global-set-key (kbd "M-n") "~")

(global-set-key (kbd "ESC M-n") "~") ;; to type ~ in a minibuffer
(global-set-key (kbd "ESC M-o") "œ")
(global-set-key (kbd "ESC M-a") "æ")

;; (global-set-key (kbd "ESC M-' a") "á")
;; (global-set-key (kbd "ESC M-' i") "í")
;; (global-set-key (kbd "ESC M-' o") "ó")
;; (global-set-key (kbd "ESC M-' u") "ú")

(global-set-key (kbd "ESC M-(") "{")
(global-set-key (kbd "ESC M-)") "}")
(global-set-key (kbd "ESC M-5") "[")
(global-set-key (kbd "ESC M-°") "]")

(global-set-key (kbd "M-(") "{")
(global-set-key (kbd "M-)") "}")
(global-set-key (kbd "M-5") "[")
;; peut poser problème, car cela supprime M-5 (qui revient +/- à C-u 5)
(global-set-key (kbd "M-°") "]")   
;; (global-set-key (kbd "C-M-)") "]") ;; alternative

(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "M-<up>") 'backward-paragraph)

;; Change window (from Wind Move built-in library) : Cmd + arrow
(windmove-default-keybindings 'super)

;; Window resize : fn + ctrl + arrow
(global-set-key (kbd "C-<prior>") 'enlarge-window)
(global-set-key (kbd "C-<next>") 'shrink-window)
(global-set-key (kbd "C-<home>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<end>") 'enlarge-window-horizontally)

(global-set-key (kbd "C-s-=") 'balance-windows)

;; Frame resize (from frame-cmds.el) : fn + alt + arrow
(global-set-key (kbd "M-<prior>")    'shrink-frame)
(global-set-key (kbd "M-<next>")  'enlarge-frame)
(global-set-key (kbd "M-<home>")  'shrink-frame-horizontally)
(global-set-key (kbd "M-<end>") 'enlarge-frame-horizontally)

;; Frame movement (from frame-cmds.el) : fn + Cmd + arrow
(global-set-key (kbd "s-<prior>") 'move-frame-up)
(global-set-key (kbd "s-<next>") 'move-frame-down)
(global-set-key (kbd "s-<home>") 'move-frame-left)
(global-set-key (kbd "s-<end>") 'move-frame-right)

;; Change frame
(global-set-key (kbd "s-<") 'other-frame)
;; (global-set-key (kbd "s->") 'other-frame)

;; Commands to minimize/maximize/normalize/fullscreen
(global-set-key (kbd "C-x -") 'suspend-frame) ;; same as C-z
(global-set-key (kbd "C-x +") 'toggle-frame-maximized) ;; same as M-f10

(defun normal-size-frame ()
  "Sets frame size to normal"
  (interactive)
  (when window-system (set-frame-size (selected-frame) 83 36)))
(global-set-key (kbd "C-x =") 'normal-size-frame)

(global-set-key (kbd "C-x _") 'toggle-frame-fullscreen) ;; same as M-f11
(global-set-key (kbd "<M-f11>") 'toggle-frame-fullscreen)

;; Zoom in/out
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Map C-z to undo (suspend-frame is now C-x -)
(global-set-key (kbd "C-z") 'undo)

;; Use smex by default on Emacs >= 24
(when (>= emacs-major-version 24)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c M-x") 'execute-extended-command))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    PARENTHESES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode 1)
(when (>= emacs-major-version 24)
  (electric-pair-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SCROLLING CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
;; mouse scrolling one/two line at a time
(setq mouse-wheel-progressive-speed nil)
;; don't accelerate mouse scrolling
(setq mouse-wheel-follow-mouse t)
;;(setq scroll-step 1)
;; keyboard scroll one line at a time
;;(setq scroll-conservatively 1000)
;; more conservative keyboard scrolling
;; (setq ring-bell-function 'ignore)    ;;enlever la ring bell

;; Move between buffers by swiping left/right (like C-x left/right)
;;(global-set-key (kbd "<wheel-right>") 'next-buffer)
;;(global-set-key (kbd "<wheel-left>") 'previous-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    IDO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
  (require 'ido-vertical-mode)
  (ido-mode 1)
  (ido-vertical-mode 1)
  (setq ido-everywhere t)
  (require 'ido-ubiquitous)
  (ido-ubiquitous-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  ;;  (setq ido-enable-flex-matching t)
					; unneeded ?  
  (setq ido-file-extensions-order '(".org" ".txt" ".tex" ".py" ".el"
				    ".ml" ".sh"))
  (setq ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./"
				 "Applications" "Calibre Library/"
				 "IdeaProjects/" "Library/" "Mail/"
				 "Movies/" "Music/" "News/" "Pictures/"
				 "Public/" "PycharmProjects/" "Sites/"))
  (defun ido-ignore-non-user-except (name)
    "Ignore all non-user (a.k.a. *starred*) buffers except for a few."
    (and (string-match "^\*" name)
	 (not (string= name "*scratch*"))
	 (not (string= name "*GNU Emacs*"))))
  (setq ido-ignore-buffers '("\\` " ido-ignore-non-user-except))
  (setq ido-ignore-extensions t)
  (setq completion-ignored-extensions '("~" ".bak" ".aux" ".out" ".bbl"
					".blg" ".fdb_latexmk" ".fls" ".gz"
					".cls" ".sty" ".pdf" ".log" ".toc"
					".DS_Store" ".mp3" ".mp4" ".mkv"
					"e.el"))
  (setq ido-use-virtual-buffers t)
					; keeps track of recently opened buffers
  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-max-saved-items 40
	recentf-exclude 
	(append recentf-exclude
		'("~/.emacs.d/el-get/" "~$" "Library/" 
		  "~/.emacs.d/elpa/" "~/.emacs.d/url/"
		  "company-statistics-cache.el")
					; does not appear to work...
		)
	)
  (setq ido-max-work-file-list 20)
  ;;(ido-sort-mtime-mode 1) ; sort files by modif time instead of alphabetically
  ;; requires that package ido-sort-mtime is installed
  )

;; C-x C-f now uses ido-find-file.
;; To leave ido, use C-f (file), C-b (buffer), C-d (dired)
;; Useful to create a new file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SPELL CHECKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "DICTIONARY" "fr")
					;(setenv "DICTIONARY" "en_US")

					;(add-hook 'TeX-language-fr-hook
					;(lambda () (ispell-change-dictionary "fr")))

					;(add-hook 'TeX-language-en-hook
					;(lambda () (ispell-change-dictionary "en_US")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    CURSOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq-default cursor-type 'bar) ;; bar, box, hbar, hollow, nil
;; (set-cursor-color 'Orange) ;; see "theme"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LINE NUMBERING AND HIGHLIGHTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
  (global-linum-mode t)
  (setq linum-format "%3d")
  ;; (setq linum-format "%4d \u2502")
  ;;
  ;; hlinum-mode extends linum-mode to highlight current line number.
  ;; Available in MELPA
  (require 'hlinum)
  (hlinum-activate)
  ;; (hlinum-deactivate) ;; to deactivate
  )

(global-hl-line-mode) ;; highlights current line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    GRAPHICAL USER INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
;; (scroll-bar-mode -1)
(if window-system
    ()
  (menu-bar-mode 0))

(setq ns-pop-up-frames nil) ;; in OS X, open new files in current frame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    FONTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-frame-font "Monaco-12")
(add-to-list 'default-frame-alist '(font . "Monaco-12"))

(defun use-djvumono ()
  "Switch the current buffer to a DejaVu Sans Mono font."
  (set-frame-font "DejaVu Sans Mono-14"))

(add-hook 'tuareg-mode-hook 'use-djvumono)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    POWERLINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (>= emacs-major-version 24) window-system)
  (require 'powerline)
  (powerline-default-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Themes
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
  ;; (require 'powerline) ;; already added
  ;; (load-theme 'misterioso t) ;; e.g. zenburn, solarized, misterioso, adwaita, gotham
  ;; waher, warm-night, zerodark.
  ;; (set-cursor-color 'Orange) ;; see "cursor". Improves misterioso
  ;; moe theme :
  (require 'moe-theme)
  ;; (moe-dark) ;; moe-dark or moe-light
  ;; (moe-theme-set-color 'orange) ;; default : blue
  ;; (require 'moe-theme-switcher) ;; light theme in the day and dark one at night
  (defun night-moe-theme ()
    (interactive)
    (moe-dark)
    (moe-theme-set-color 'blue))

  (defun day-moe-theme ()
    (interactive)
    (moe-light)
    (moe-theme-set-color 'orange))

  (defun switch-moe-theme ()
    (let ((now (string-to-number (format-time-string "%H"))))
      ;;      (if (and (>= now 07) (<= now 18)) ;; day between 7am and 7pm
      (if (and (>= now 07) (<= now 23)) ;; day between 7am and 11pm
	  (day-moe-theme)
	(night-moe-theme))
      nil))

  (switch-moe-theme)
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    FRAME SIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when window-system (add-to-list 'default-frame-alist '(width . 83)))
;; (add-to-list 'default-frame-alist '(height . 33))
(when window-system (set-frame-size (selected-frame) 83 36))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    BACKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change backup directory
(setq backup-directory-alist `(("." . "~/.backups")))

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 6   ;; Number of newest versions to keep.
      kept-old-versions 2   ;; Number of oldest backups to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      ;; backup-by-copying t  ;; Copy all files, don't rename them.
      delete-by-moving-to-trash t
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'org-mode-hook 'use-)
;; M-<left/right> org-do-promote/demote

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AucTeX : compiler en pdf par défaut

(setq TeX-PDF-mode t)

;; Other AucTeX configuration
;; see http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/

;; (setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil) ;; autosave before compiling
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
(server-start); start emacs in server mode so that skim can talk to it

;; ~/.latexmkrc :
;; $pdflatex = 'pdflatex -interaction=nonstopmode -synctex=1 %O %S';
;; $pdf_previewer = 'open -a skim';
;; $clean_ext = 'bbl rel %R-blx.bib %R.synctex.gz';

;; Utiliser C-c = pour utiliser reftex et naviguer rapidement dans le
;; document (utiliser TAB ou ENTER sur une section pour s'y rendre).
;; Dans Skim, utiliser Cmd+Shift+Click pour aller à la partie correspondante.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SHELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adapter la variable d'environnement $PATH sous Emacs, qui contient moins de
;; chemins que sous le terminal.

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used 
by the user's shell. This is particularly useful under Mac OSX, where GUI apps
are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; puis lancer la fonction 'set-exec-path-from-shell-PATH' au début de chaque
;; session.

(set-exec-path-from-shell-PATH)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    AUTOCOMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alternative : either auto-complete-mode, or company-mode

(when (>= emacs-major-version 24)
  (autoload 'company-mode "company" nil t)
  (global-company-mode t)
  (setq company-idle-delay 0)
  ;; (add-hook 'after-init-hook 'company-statistics-mode)
  ;; uses stats for better auto-completion suggestions
  ;; (setq company-minimum-prefix-length 2) ;; starts autocomplettion sooner
  (add-hook 'after-init-hook 'company-statistics-mode)
  )

;; (require 'auto-complete)
;; (global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    MERLIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Take a look at https://github.com/the-lambda-church/merlin for more
;; information

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    CUSTOM 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Try setting themes manually instead of using "custom"
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ido-sort-mtime ido-ubiquitous zerodark-theme zenburn-theme yasnippet warm-night-theme waher-theme s powerline popup moe-theme hlinum gotham-theme company-statistics caml auctex aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
