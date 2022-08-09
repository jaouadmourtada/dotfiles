;; Emacs setup.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SECTIONS :
;;;;
;;;;    STARTUP
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
;;;;    AUTO-REVERT
;;;;    ORG
;;;;    AUCTEX
;;;;    IMENU
;;;;    SHELL
;;;;    PYTHON
;;;;    AUTOCOMPLETE
;;;;    YASNIPPET
;;;;    MARKDOWN
;;;;    MERLIN
;;;;    CUSTOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Type C-x C-e at the end of an expression to evaluate it

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    STARTUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get rid of the starting screen
(setq inhibit-splash-screen t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LOAD PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell emacs where your personal elisp library directory is :
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; no longer necessary


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    ELPA, MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
;; (package-initialize) ; de-activated since emacs 27:
;; Warning (package): Unnecessary call to ‘package-initialize’ in init file


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LOAD PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load the packaged named xyz :
;; (load "xyz") ;; best not to include the ending “.el” or “.elc”

;; with autoloads (lazy loading; unsure if works)
;;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;;; (autoload 'ocamldebug "ocamldebug" "Run the Caml debugger" t)
;;(autoload 'tuareg_indent "tuareg_indent" t)
;;(autoload 'tuareg-site-file "tuareg-site-file" t)

;; without autoloads (works but slow)
;;(load "tuareg")
;;(load "ocamldebug")
;;(load "tuareg_indent")
;;(load "tuareg-site-file")

;; aggressive-indent mode
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;; enable one
(global-aggressive-indent-mode 1) ;; enable all
;; (add-to-list 'aggressive-indent-excluded-modes 'shell-script-mode)
;; disable one

;; (load "minimap") ;; M-x minimap-mode to activate minimap.
;; add a hook (length <- 108) for minimap-mode.

;; Auto-start Smex every time you open Emacs.
(load "smex")
(require 'smex) ;; Not needed if you use package.el
(smex-initialize) ;; Can be omitted. This might cause a (minimal) delay
;; when Smex is auto-initialized on its first run.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    KEY BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Special characters

(setq ns-right-alternate-modifier nil)
;; right alt is no longer a meta key !!
;; note that the following shortcuts now become virtually useless

;; kill line backwards (inverse of C-k, with s-backspace)
(global-set-key (kbd "s-<backspace>") (kbd "M-0 C-k"))

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

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

(global-set-key (kbd "M-(") "{")
(global-set-key (kbd "M-)") "}")
(global-set-key (kbd "M-5") "[")
(global-set-key (kbd "M-°") "]")   

(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "M-<up>") 'backward-paragraph)

;; Change window (from Wind Move built-in library) : Cmd + arrow
(windmove-default-keybindings 'super)

;; Change buffer
(global-set-key (kbd "C-M-<tab>") 'next-buffer)
(global-set-key (kbd "C-M-S-<tab>") 'previous-buffer)

;; Change frame
(global-set-key (kbd "M-s-<right>") 'other-frame)
(global-set-key (kbd "M-s-<left>") 'other-frame)

;; Window resize : fn + ctrl + arrow
(global-set-key (kbd "C-<prior>") 'enlarge-window)
(global-set-key (kbd "C-<next>") 'shrink-window)
(global-set-key (kbd "C-<home>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<end>") 'enlarge-window-horizontally)

(global-set-key (kbd "C-s-=") 'balance-windows)

;;;; Frames movements
;;(load "frame-fns")
;;(load "frame-cmds")
;;
;;;; Frame resize (from frame-cmds.el) : fn + alt + arrow
;;(global-set-key (kbd "M-<prior>")    'shrink-frame)
;;(global-set-key (kbd "M-<next>")  'enlarge-frame)
;;(global-set-key (kbd "M-<home>")  'shrink-frame-horizontally)
;;(global-set-key (kbd "M-<end>") 'enlarge-frame-horizontally)
;;
;;;; Frame movement (from frame-cmds.el) : fn + Cmd + arrow
;;(global-set-key (kbd "s-<prior>") 'move-frame-up)
;;(global-set-key (kbd "s-<next>") 'move-frame-down)
;;(global-set-key (kbd "s-<home>") 'move-frame-left)
;;(global-set-key (kbd "s-<end>") 'move-frame-right)

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

;;; Zoom in/out (text scale)
;; default-text-scale mode: change text scale globally (add 1 size)
(global-set-key (kbd "C-+") 'default-text-scale-increase)
(global-set-key (kbd "C--") 'default-text-scale-decrease)
(global-set-key (kbd "C-=") 'default-text-scale-reset)
;; text-scale-increase below is buffer-specific rather than global/default (less useful/deprecated)
;; (setq text-scale-mode-step 1.07) ; scale size by 1.07 instead of 1.2 factor
;; (global-set-key (kbd "C-x C-+") 'text-scale-increase)
;; (global-set-key (kbd "C-x C--") 'text-scale-decrease)

;; Map C-z to undo (suspend-frame is now C-x -)
(global-set-key (kbd "C-z") 'undo)

;; Use smex by default
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; use count-words instead of count-words-region as it works on buffer
;; if no region is selected
(global-set-key (kbd "M-=") 'count-words)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    PARENTHESES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode 1)
(electric-pair-mode 1)

;; helps match braces '\{ \}'
;; "After evaluation of this form, \ acts as a punctuation character rather than an escape character", see https://tex.stackexchange.com/questions/74100/matching-the-delimiters-and-in-auctex
;; ! may lead to unexpected side effects
;; ideally, one should still treat '\' as an escape character, except when followed by {}
;; (add-hook 'TeX-mode-hook (lambda () (modify-syntax-entry ?\\ ".")))

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

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-everywhere t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
;;  (setq ido-enable-flex-matching t)		; unneeded ?  
(setq ido-file-extensions-order '(".org" ".txt" ".tex" ".py" ".el"
				  ".ml" ".sh"))
(setq ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./"
			       "Applications" "Calibre Library/"
			       "Library/" "Mail/" "Movies/"
			       "Music/" "News/" "Pictures/"
			       "Public/" "Sites/"))
(defun ido-ignore-non-user-except (name)
  "Ignore all non-user (a.k.a. *starred*) buffers except for a few."
  (and (string-match "^\*" name)
       (not (string= name "*scratch*"))
       (not (string= name "*GNU Emacs*"))))
(setq ido-ignore-buffers '("\\` " ido-ignore-non-user-except))
(setq ido-ignore-extensions t)
(setq completion-ignored-extensions '("~" ".bak" ".aux" ".out" ".bbl"
				      ".blg" ".fdb_latexmk" ".fls" ".gz"
				      ".cls" ".sty" ".log" ".pdf" ".toc"
				      ".snm" ".nav" ".maf" ".html"
				      ".djvu" ".docx" ".xls" ".webloc" ".gcx"
				      ".png" ".jpg" ".jpeg" ".webp" ".gif"
				      ".DS_Store" ".mp3" ".mp4" ".mkv" ".PNG"
				      "e.el" ".localized" ".zip"))
(setq ido-use-virtual-buffers t)   ; keeps track of recently opened buffers
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 80
      recentf-exclude 
      (append recentf-exclude
	      '("~/.emacs.d/el-get/" "~$" "Library/" 
		"~/.emacs.d/elpa/" "~/.emacs.d/url/"
		"company-statistics-cache.el"
		"[:ascii:]*loads.el"
		"\\.log$"
		"\\.aux$"
		"\\.toc$"
		)
	      )
      )
(setq ido-max-work-file-list 20)
;;(ido-sort-mtime-mode 1) ; sort files by modif time instead of alphabetically
;; requires that package ido-sort-mtime is installed

;; C-x C-f now uses ido-find-file.
;; To leave ido, use C-f (file), C-b (buffer), C-d (dired)
;; Useful to create a new file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SPELL CHECKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ispell-local-dictionary-alist
      ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
      ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
	("fr" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr") nil utf-8)
	)
      )
(setq ispell-local-dictionary "en_US")
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=fast" "--lang=en_US"))

;; short commands to change dictionaries
(defun english ()
  (interactive)
  (ispell-change-dictionary "en_US")
  )

(defun french ()
  (interactive)
  (ispell-change-dictionary "fr")
  )

;; Emacs 24.4 considerably improves support for Hunspell, and is now
;; able to use Hunspell automatically with only little further
;; customization. Notably, Emacs can now discover available Hunspell
;; dictionaries, and fills ispell-dictionary-alist
;; automatically. Essentially, you just need the following to tell
;; Emacs to use hunspell:
;; (setq ispell-program-name (executable-find "hunspell"))

;; ;; Path of dictionaries (?)
;; (setenv "DICPATH" (concat (getenv "HOME") "/Library/Spelling"))
;; ;; (setenv "DICPATH" "~/Library/Spelling")

;; Besides language-specific dictioniaries, you also need to have a
;; “default” dictionary for Emacs. This dictionary needs to be named
;; default, literally. Creating it is easy enough, though. Just create
;; symlinks to the dictionaries of your preferred language:
;; 
;; $ cd ~/Library/Spelling
;; $ ln -s en_GB.aff default.aff
;; $ ln -s en_GB.dic default.dic

;;(add-hook 'TeX-language-fr-hook
;;	  (lambda () (ispell-change-dictionary "fr")))

;;(add-hook 'TeX-language-en-hook
;;	  (lambda () (ispell-change-dictionary "en_US")))

;; new code / ad-hoc
;; (setenv "DICPATH" (concat (getenv "HOME") "~/Library/Spelling"))
;; Tell ispell-mode to use hunspell.
;; (setq ispell-program-name "/usr/local/bin/hunspell")

;; Activate flyspell
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; (setq ispell-local-dictionary "en_US") ;; english dictionary by default
;; (setq ispell-local-dictionary "fr")
;; (setq ispell-local-dictionary-alist
;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
;; '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
;;  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    CURSOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq-default cursor-type 'bar) ;; bar, box, hbar, hollow, nil
;; (set-cursor-color 'Orange) ;; see "theme"

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LINE NUMBERING AND HIGHLIGHTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; linum-mode leads to a bug in Company where the cursor jumps accross the screen
;; It also leads to performance issues when used with folding
;; As a result, it is probably best turned off
;; (global-linum-mode t)
;; (setq linum-format "%3d")
;; ;; (setq linum-format "%4d \u2502")

;; A better alternative is display-line-numbers-mode, which does not have these issues, although it takes up more space
;; (global-display-line-numbers-mode)

;; hlinum-mode extends linum-mode to highlight current line number.
;; Available in MELPA
(require 'hlinum)
(hlinum-activate)
;; (hlinum-deactivate) ;; to deactivate
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
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-13"))
;; On Ubuntu:
;; (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))
;; other fonts : liberation mono, menlo, monaco

(defun use-djvumono ()
  "Switch the current buffer to a DejaVu Sans Mono font."
  (set-frame-font "DejaVu Sans Mono-14"))

(add-hook 'tuareg-mode-hook 'use-djvumono)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    POWERLINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (require 'powerline)
  (powerline-default-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
;; (require 'powerline) ;; already added
;; (load-theme 'misterioso t) ;; e.g. zenburn, solarized, misterioso, adwaita, gotham, waher, warm-night, zerodark.
;; (set-cursor-color 'Orange) ;; see "cursor". Improves misterioso
(require 'moe-theme)
;; (moe-dark) ;; moe-dark or moe-light
;; (moe-theme-set-color 'orange) ;; default : blue
;; (require 'moe-theme-switcher) ;; day: light theme, night: dark theme

;; after moe-theme update: replace set-color by apply-color
(defun night-moe-theme ()
  (interactive)
  (moe-dark)
  ;;(moe-theme-set-color 'blue)
  (moe-theme-apply-color 'blue)
  )

;; after moe-theme update: replace set-color by apply-color
(defun day-moe-theme ()
  (interactive)
  (moe-light)
  ;;(moe-theme-set-color 'orange)
  (moe-theme-apply-color 'orange)  
  )

(moe-light)
;;(moe-theme-set-color 'orange)
(moe-theme-apply-color 'orange)
;;(moe-theme-select-color 'orange) ;; interactive


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    FRAME SIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when window-system (add-to-list 'default-frame-alist '(width . 83)))
;; (add-to-list 'default-frame-alist '(height . 33))
(when window-system (set-frame-size (selected-frame) 83 36))

;; zoom in/out: globally change text size
(defadvice text-scale-increase (around all-buffers (arg) activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

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
;;;;    AUTO-REVERT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Refresh buffers when the underlying file has changed
;; useful when using git or Bibdesk
(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'org-mode-hook 'use-)
;; M-<left/right> org-do-promote/demote

(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))
(global-set-key (kbd "C-c a") 'org-agenda)

;; turn off linum mode in org mode
;; no longer needed as linum-mode is now turned off by default
;; (defun turn-off-linum-mode ()
;;   (linum-mode -1))
;; (add-hook 'org-mode-hook 'turn-off-linum-mode)

;; C-up and down to navigate
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-<up>") 'org-previous-visible-heading)
  (define-key org-mode-map (kbd "C-<down>") 'org-next-visible-heading)
  ;; Add other org commands
  )

;; open files folded by default
(setq org-startup-folded t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AucTeX : compiler en pdf par défaut

(setq TeX-PDF-mode t)

;; Other AucTeX configuration
;; see http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/

;; (setq TeX-auto-save t) ;; creates 'auto' subdirectory to
;; keep information about macros for multifile projects.
;; Can be enabled on a per-file basis by adding
;; %%% TeX-auto-save: t
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil) ;; autosave before compiling
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode) ;; done in "SPELL CHECKING"
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; automatically pair dollars
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)))

;; automatically pair
;; (defvar latex-electric-pairs '(
;; 			       (?\\\{ . ?\\\})
;;     			       (?\\\[ . ?\\\])
;; 			       ) "Electric pairs for LaTeX.")
;; (defun latex-add-electric-pairs ()
;;   (setq-local electric-pair-pairs (append electric-pair-pairs latex-electric-pairs))
;;   (setq-local electric-pair-text-pairs electric-pair-pairs))
;; (add-hook 'LaTeX-mode-hook 'latex-add-electric-pairs)

;; Add backends provided by company-auctex to company-backends
(require 'company-auctex)
(company-auctex-init)

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

;; RefTeX : add environment
(setq reftex-label-alist
      '(("lemma"   ?l "lem:"  "~\\ref{%s}" nil ("lemma" "lemme"))
      	("theorem" ?t "thm:" "~\\ref{%s}" t ("theorem" "théorème") -3)
      	("corollary" ?c "cor:" "~\\ref{%s}" t ("corollary" "corollaire") -2)
      	("proposition" ?p "prop:" "~\\ref{%s}" t ("proposition"))
      	("definition" ?d "def:" "~\\ref{%s}" t ("definition" "définition"))
	("remark" ?r "rem:" "~\\ref{%s}" nil ("remark" "remarque"))
	("example" ?x "ex:" "~\\ref{%s}" nil ("example" "exemple"))
	("assumption" ?a "ass:"  "~\\ref{%s}" nil ("assumption"))
	))

;; automatically add label when environment is created
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (LaTeX-add-environments
	     '("lemma" LaTeX-env-label)
	     '("theorem" LaTeX-env-label)
	     '("corollary" LaTeX-env-label)
	     '("proposition" LaTeX-env-label)
	     '("definition" LaTeX-env-label)
	     )))

(eval-after-load "tex-ispell"
  '(progn
     (TeX-ispell-skip-setcar
      '(("\\\\jao" ispell-tex-arg-end)
	("\\\\citet" ispell-tex-arg-end)
	("\\\\citep" ispell-tex-arg-end)
	("\\\\eqref" ispell-tex-arg-end)
	))
     ;;(TeX-ispell-skip-setcdr
     ;; ’(
     ;; ("list" ispell-tex-arg-end 2)
     ;; ("" . "\\\\end{myverbatim}")
     ;; ))
     ))

;; folding sections and navigation

(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

(eval-after-load 'outline
  '(progn
     (require 'outline-magic)
     (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)
     (define-key outline-minor-mode-map (kbd "C-c C-u") 'outline-up-heading)
     (define-key outline-minor-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
     (define-key outline-minor-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
     (define-key outline-minor-mode-map (kbd "C-c <C-down>") 'outline-next-visible-heading)
     (define-key outline-minor-mode-map (kbd "C-c <C-up>") 'outline-previous-visible-heading)
     (define-key outline-minor-mode-map (kbd "C-c <C-right>") 'outline-forward-same-level)
     (define-key outline-minor-mode-map (kbd "C-c <C-left>") 'outline-backward-same-level)
     ))

;;(setq TeX-outline-extra
;;      ;; other TeX outline levels
;;      '(("[ \t]*\\\\\\(bib\\)?item\\b" 7)
;;	("\\\\bibliography\\b" 2)))

;; Insert new entry to the cv bibliography
(defun cvpub ()
  "Add a new publication."
  (interactive) ;; function is a command, can be called by M-x
  (insert "\n% ================\n\\cvpub%\n"
	  "[" (read-string "STATUS: preprint | major | minor | accepted | published: [preprint] " nil nil "preprint") "]% status\n"
	  ;; ido-completing-read?
	  "{" (read-string "Year: [2021] " nil nil "2021") "}% year\n"
	  "{" (read-string "Title: ") "}% title\n"
	  "{" (read-string "Authors: [J. Mourtada] " nil nil "J. Mourtada") "}% authors\n"
	  "{" (read-string "arXiv id: ") "}% arXiv id\n"
	  "{" (read-string "Venue [empty unless revision]: ") "}% venue\n"
	  "{" (read-string "Volume(number):pages [empty unless published]: ") "}% volume(number):pages\n"
	  "{" (read-string "Link [empty unless published]:") "}% URL link\n"
	  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    IMENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; code navigation
(global-set-key (kbd "C-c i") 'imenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SHELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure environment variables inside GUI Emacs in OS X are the same
;; as in the user's shell

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use elpy for python
(package-initialize)
(elpy-enable)

(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

;; use ipython (instead of python) as the default shell
;; make sure ipython is installed
;; (elpy-use-ipython) ; doesn't work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    AUTOCOMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alternative : either auto-complete-mode, or company-mode

(autoload 'company-mode "company" nil t)
(global-company-mode t)
(setq company-idle-delay 0)
;; (add-hook 'after-init-hook 'company-statistics-mode)
;; uses stats for better auto-completion suggestions
(setq company-minimum-prefix-length 2) ;; starts autocompletion sooner
(add-hook 'after-init-hook 'company-statistics-mode)

;; (require 'auto-complete)
;; (global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    YASNIPPET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq yas-snippet-dirs '("~/emacs.d/snippets/"))
(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already
(eval-after-load 'yasnippet
  '(progn          
     (define-key yas-keymap (kbd "C-o") 'yas-expand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq markdown-coding-system "utf-8")
(setq markdown-xhtml-header-content
      (concat "<script type=\"text/javascript\" async"
              " src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/"
              "2.7.1/MathJax.js?config=TeX-MML-AM_CHTML\">"
              "</script>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JEMDOC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use jemdoc-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'company) 
(require 'company-web-html)  ; load company mode html backend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    MERLIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add opam emacs directory to the load-path
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var"
						     "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))
;; Take a look at https://github.com/the-lambda-church/merlin for more
;; information

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    CUSTOM 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Momentary
(add-hook 'text-mode-hook 'visual-line-mode)

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Try setting themes manually instead of using "custom"
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "a53f00556ab4c81a0618ab6589053d9e351312d37d9c9cf544e0c8edac2b63ab" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "0feb7052df6cfc1733c1087d3876c26c66410e5f1337b039be44cb406b6187c6" default))
 '(markdown-command "/usr/local/bin/pandoc")
 '(org-agenda-files
   '("~/Dropbox/taf/todo-these.org" "~/Dropbox/perso/todo-new.org" "~/Dropbox/perso/todo-projects.org"))
 '(package-selected-packages
   '(company with-editor powerline solarized-theme magit default-text-scale outline-magic yasnippet company-web company-auctex web-mode tuareg markdown-mode elpy exec-path-from-shell ido-sort-mtime ido-ubiquitous ido-vertical-mode s smex moe-theme hlinum company-statistics caml auctex aggressive-indent frame-fns frame-cmds outline-magic ido-completing-read+))
 '(safe-local-variable-values '((TeX-parse-self . t) (TeX-auto-save . t))))
;; also: waher-theme, yasnippet, warm-night-theme, gotham-theme,
;; zerodark-theme, zenburn-theme, popup
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
