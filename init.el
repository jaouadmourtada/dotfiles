;; Emacs setup.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SECTIONS :
;;;;
;;;;    STARTUP
;;;;    LOAD PATH
;;;;    ELPA, MELPA
;;;;    LOAD PACKAGES
;;;;    KEY BINDINGS
;;;;    PARENTHESES
;;;;    SCROLLING CUSTOMIZATION
;;;;    VERTICO, CONSULT, ORDERLESS, PRESCIENT
;;;;    SPELL CHECKING
;;;;    AUTOCOMPLETE
;;;;    YASNIPPET
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
;;;;    CITAR
;;;;    IMENU
;;;;    SHELL
;;;;    PYTHON
;;;;    MARKDOWN
;;;;    OCAML
;;;;    CUSTOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Type C-x C-e at the end of an expression to evaluate it

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    STARTUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get rid of the starting screen
(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LOAD PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell emacs where your personal elisp library directory is :
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; no longer necessary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    ELPA, MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    USE-PACKAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/use-package/")
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LOAD PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load the packaged named xyz :
;; (load "xyz") ;; best not to include the ending “.el” or “.elc”

;; with autoloads (lazy loading; unsure if works)
;;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)

;; without autoloads (works but slow)
;;(load "tuareg")
;; tuareg, ocamldebug, tuareg_indent, tuareg-site-file

;; aggressive-indent mode
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  )
;; ;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;; enable one
;; (global-aggressive-indent-mode 1) ;; enable all
;; ;; (add-to-list 'aggressive-indent-excluded-modes 'shell-script-mode)
;; ;; disable one

;; (load "minimap") ;; M-x minimap-mode to activate minimap.
;; add a hook (length <- 108) for minimap-mode.

;; Auto-start Smex every time you open Emacs.
;; no longer needed/used
;; (use-package smex
;;   :ensure t
;;   :config
;;   (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;   ;; when Smex is auto-initialized on its first run.
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    KEY BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Special characters

(setq ns-right-alternate-modifier nil)
;; right alt is no longer a meta key !!
;; note that the following shortcuts now become virtually useless

;; kill line backwards (inverse of C-k, with s-backspace)
(global-set-key (kbd "s-<backspace>") (kbd "M-0 C-k"))

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(global-set-key (kbd "M-q") 'indent-region)

;; (defun unfill-paragraph ()
;;   (interactive)
;;   (let ((fill-column (point-max)))
;;     (fill-paragraph nil)))
;; (global-set-key (kbd "M-Q") 'unfill-paragraph)

(global-set-key (kbd "M-/") "\\")
(global-set-key (kbd "M-L") "|")
(global-set-key (kbd "M-n") "~")

;; to type ~ in a minibuffer (not needed on macos)
;; (global-set-key (kbd "ESC M-n") "~") 
;; (global-set-key (kbd "ESC M-o") "œ")
;; (global-set-key (kbd "ESC M-a") "æ")

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
(use-package default-text-scale
  :ensure t
  :bind (("C-+" . default-text-scale-increase)
         ("C--" . default-text-scale-decrease)
         ("C-=" . default-text-scale-reset))
  )

;; text-scale-increase below is buffer-specific rather than global/default (less useful/deprecated)
;; (setq text-scale-mode-step 1.07) ; scale size by 1.07 instead of 1.2 factor
;; (global-set-key (kbd "C-x C-+") 'text-scale-increase)
;; (global-set-key (kbd "C-x C--") 'text-scale-decrease)

;; Map C-z to undo (suspend-frame is now C-x -)
(global-set-key (kbd "C-z") 'undo)

;; use count-words instead of count-words-region as it works on buffer
;; if no region is selected
(global-set-key (kbd "M-=") 'count-words)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    PARENTHESES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode 1)
(electric-pair-mode 1)

;; helps match braces '\{ \}'
;; "After evaluation of this form, \ acts as a punctuation character rather than an escape character", see https://tex.stackexchange.com/questions/74100/matching-the-delimiters-and-in-auctex
;; ! may lead to unexpected side effects
;; ideally, one should still treat '\' as an escape character, except when followed by {}
;; (add-hook 'TeX-mode-hook (lambda () (modify-syntax-entry ?\\ ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SCROLLING CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    RECENTF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Useful for completion

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 90
      recentf-exclude 
      (append recentf-exclude
	      '("~/.emacs.d/el-get/" "~$" "Library/"
		"/Applications/"
		"~/.emacs.d/elpa/" "~/.emacs.d/url/"
		"company-statistics-cache.el"
		"/usr/" "~/.emacs.d/elpa"
		"[:ascii:]*loads.el" ".DS_Store"
		"\\.log$" "\\.aux$" "\\.toc$"
		"\\.sty$" "\\.cls$" "\\.clo$"
		"\\.vrb$" "\\.gz$" "\\.html"
		"\\.bbl$")
	      )
      )

(setq completion-ignored-extensions '("~" ".bak" ".aux" ".out" ".bbl"
				      ".blg" ".fdb_latexmk" ".fls" ".gz"
				      ".cls" ".sty" ".log" ".pdf" ".toc"
				      ".snm" ".nav" ".maf" ".vrb" ".html"
				      ".djvu" ".docx" ".xls" ".webloc" ".gcx"
				      ".png" ".jpg" ".jpeg" ".webp" ".gif"
				      ".DS_Store" ".mp3" ".mp4" ".mkv" ".PNG"
				      "e.el" ".localized" ".zip"
				      "auto/" ".git/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x d") 'dired-jump)

(defun quit-window-kill-buffer ()
  "Quit the window but also kill the corresponding buffer."
  (interactive)
  (quit-window t))

(define-key Info-mode-map (kbd "q") 'quit-window-kill-buffer)
(define-key Buffer-menu-mode-map (kbd "q") 'quit-window-kill-buffer)
;;(define-key debugger-mode-map (kbd "q") 'quit-window-kill-buffer)

(add-hook 'help-mode-hook
	  (lambda ()
	    (define-key help-mode-map (kbd "q") 'quit-window-kill-buffer))
	  )

(add-hook 'special-mode-hook
	  (lambda ()
	    (define-key special-mode-map (kbd "q") 'quit-window-kill-buffer))
	  )

(add-hook 'magit-mode-hook
	  (lambda ()
	    (define-key magit-revision-mode-map (kbd "q") 'quit-window-kill-buffer))
	  )

;; remove the one below?
(add-hook 'magit-status-mode-hook
	  (lambda ()
	    (define-key magit-status-mode-map (kbd "q") 'quit-window-kill-buffer))
	  )

(add-hook 'magit-diff-mode-hook
	  (lambda ()
	    (define-key magit-diff-mode-map (kbd "q") 'quit-window-kill-buffer))
	  )

(add-hook 'magit-process-mode-hook
	  (lambda ()
	    (define-key magit-process-mode-map (kbd "q") 'quit-window-kill-buffer))
	  )

(add-hook 'completion-list-mode
	  (lambda ()
	    (define-key completion-list-mode-map (kbd "q") 'quit-window-kill-buffer))
	  )

(add-hook 'reftex-toc-mode-hook
	  (lambda ()
	    (define-key reftex-toc-mode-map (kbd "q") 'quit-window-kill-buffer))
	  )

(with-eval-after-load "dired"
  ;; Add other dired commands
  ;; remove some if not needed
  (define-key dired-mode-map (kbd "q") 'quit-window-kill-buffer)
  (define-key dired-mode-map (kbd "b") 'dired-up-directory)
  (define-key dired-mode-map (kbd "u") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C-c C-u") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C-<up>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "-") 'dired-unmark)
  (define-key dired-mode-map (kbd "C-<down>") 'dired-find-file)
  (define-key dired-mode-map (kbd "<tab>") 'dired-next-dirline)
  (define-key dired-mode-map (kbd "S-<tab>") 'dired-prev-dirline)
  (define-key dired-mode-map (kbd "C-n") 'dired-next-dirline)
  (define-key dired-mode-map (kbd "C-p") 'dired-prev-dirline)
  (define-key dired-mode-map (kbd ":") 'dired-isearch-filenames)
  )

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
;; use parenthesis to show details


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    VERTICO, CONSULT, ORDERLESS, PRESCIENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for more consult tricks: https://www.youtube.com/watch?v=HwBHBwYgs2g

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("<right>" . vertico-next)
              ("<left>" . vertico-previous)
              ("C-f" . vertico-exit))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :ensure t
  :init
  (setq consult-buffer-filter
	'("\\` " "Completions\\*\\'" "\\`\\*Flymake log\\*\\'" "\\`\\*Semantic SymRef\\*\\'" "\\`\\*tramp/.*\\*\\'" ;; "\\`\\*Messages\\*\\'"
	  "Output\\*\\'" "output\\*\\'" "RefTeX" "\\*toc\\*" "Help\\*" "*Buffer List*" "*Backtrace*" "*Directory*" "magit-process:" "magit-diff:"))
  :bind (("C-x b" . consult-buffer)
	 ("M-s" . consult-line) ; previously: M-s l
	 ("M-o" . consult-outline) ; previously: M-s o
	 )
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package vertico-prescient
  :ensure t
  ;; :init
  ;; (vertico-prescient-mode)
  )

;; make find-file behave like ido-find-file

;; C-DEL moves up a directory
;;(define-key vertico-map (kbd "C-<backspace>") 'vertico-directory-up) ;;test
;;(define-key vertico-map (kbd "M-<backspace>") 'vertico-directory-up)

;; RET does not open directory in dired
(define-key vertico-map (kbd "RET") 'vertico-directory-enter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SPELL CHECKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    AUTOCOMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; quick fix to company not loading properly in auctex?

;; (autoload 'company-mode "company" nil t)
;; (global-company-mode t)
;; (setq company-idle-delay 0)
;; (setq company-minimum-prefix-length 2) ;; starts autocompletion sooner
;; (add-hook 'after-init-hook 'company-statistics-mode)

;; company-mode
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2) ;; starts autocompletion sooner
  ;; uses stats for better auto-completion suggestions
  )

;; company-statistcs: uses stats for better auto-completion suggestions
(use-package company-statistics
  :ensure t
  ;; :pin gnu
  :init
  (add-hook 'after-init-hook 'company-statistics-mode)
  )

;; ;; Enable corfu globally
;; (use-package corfu
;;   :ensure t
;;   :init
;;   (global-corfu-mode)
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for completions
;;   (corfu-auto t)                 ;; Enable auto-completion
;;   (corfu-auto-prefix 2)          ;; Minimum prefix length before auto-completion starts
;;   (corfu-auto-delay 0.1)         ;; Faster auto-completion
;;   (corfu-popupinfo-mode t)       ;; Enable documentation popup
;;   (corfu-preselect 'directory) ;; Select the first candidate, except for directories
;;   :config
;;   (setq corfu-quit-no-match 'separator)  ;; Avoid annoying completion exit
;;   (setq corfu-preview-current nil)       ;; Don't preview the current completion candidate
;;   (setq corfu-preselect 'prompt)         ;; Preselect the first candidate
;;   (setq corfu-popupinfo-delay '(0.5 . 1.0))  ;; Delay for popups
;;   (setq corfu-popupinfo-direction 'bottom)  ;; Popup below the completion menu  
;;   (corfu-popupinfo-mode 1)                ;; Show inline documentation
;;   (unless (display-graphic-p)
;;     (use-package corfu-terminal
;;       :ensure t
;;       :config
;;       (corfu-terminal-mode +1))))  ;; Enable corfu in terminal mode

;; ;; Cape provides additional completion backends
;; (use-package cape
;;   :ensure t
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)  ;; Dynamic completion
;;   (add-to-list 'completion-at-point-functions #'cape-file)     ;; File path completion
;;   (add-to-list 'completion-at-point-functions #'cape-tex)      ;; TeX-specific completion
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))  ;; Symbol completion

;; ;; ;; Enable corfu in text-mode and LaTeX mode
;; ;; (add-hook 'text-mode-hook #'corfu-mode)
;; ;; (add-hook 'latex-mode-hook #'corfu-mode)

;; ;; Use TAB for completion (optional)
;; (define-key corfu-map (kbd "TAB") 'corfu-next)
;; (define-key corfu-map (kbd "<backtab>") 'corfu-previous)

;; (provide 'corfu-config)

;; (use-package corfu
;;   :ensure t
;;   ;; Optional customizations
;;   :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   (corfu-auto t)               ;; Enable auto completion
;;   (corfu-preselect 'directory) ;; Select the first candidate, except for directories
;;   (corfu-quit-no-match 'separator) ;; Configure quitting
;;   (corfu-auto-delay  0)  ;; TOO SMALL - NOT RECOMMENDED
;;   (corfu-auto-prefix 0) ;; TOO SMALL - NOT RECOMMENDED

;;   ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
;;   ;; be used globally (M-/).  See also the customization variable
;;   ;; `global-corfu-modes' to exclude certain modes.
;;   :init
;;   (global-corfu-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    YASNIPPET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  ;; :pin gnu
  :config
  (yas-global-mode 1)
  ;; or M-x yas-reload-all if you've started YASnippet already
  ;; (setq yas-snippet-dirs '("~/emacs.d/snippets/"))
  ;;;  :bind-keymap
  :bind
  ("C-o" . yas-expand)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    CURSOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq-default cursor-type 'bar) ;; bar, box, hbar, hollow, nil
;; (set-cursor-color 'Orange) ;; see "theme"

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Momentary
(add-hook 'text-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LINE NUMBERING AND HIGHLIGHTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; linum-mode leads to a bug in Company where cursor jumps accross the screen
;; It also leads to performance issues when used with folding
;; As a result, it is probably best turned off (also it is deprecated)

;; more modern alternative: nlinum-mode; does not lead to the previous bug
(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode t)
  )

;; A better alternative is display-line-numbers-mode, which does not have these issues, although it takes up more space
;; (global-display-line-numbers-mode)

(use-package hlinum
  ;; highlight current line number
  :ensure t
  :config
  (hlinum-activate)
  ;; hlinum-deactivate to deactivate
  )

(global-hl-line-mode) ;; highlights current line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    GRAPHICAL USER INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
;; (scroll-bar-mode -1)
(if window-system
    ()
  (menu-bar-mode 0))

(setq ns-pop-up-frames nil) ;; in OS X, open new files in current frame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    FONTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-frame-font "Monaco-12")
(add-to-list 'default-frame-alist '(font . "Monaco-12"))
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-13"))
;; On Ubuntu:
;; (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))
;; other fonts : liberation mono, menlo, monaco

(defun use-djvumono ()
  "Switch the current buffer to a DejaVu Sans Mono font."
  (set-frame-font "DejaVu Sans Mono-14"))

;; (add-hook 'tuareg-mode-hook 'use-djvumono)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    POWERLINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package powerline
  :if window-system
  :ensure t
  :config
  (powerline-default-theme)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
;; (load-theme 'misterioso t) ;; e.g. zenburn, solarized, misterioso, adwaita, gotham, waher, warm-night, zerodark.
;; (set-cursor-color 'Orange) ;; see "cursor". Improves misterioso

(use-package moe-theme
  :ensure t
  :config
  (moe-light)
  (moe-theme-apply-color 'orange)
  (defun night-moe-theme ()
    (interactive)
    (moe-dark)
    (moe-theme-apply-color 'blue))
  (defun day-moe-theme ()
    (interactive)
    (moe-light)
    (moe-theme-apply-color 'orange))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    FRAME SIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when window-system (add-to-list 'default-frame-alist '(width . 83)))
;; (add-to-list 'default-frame-alist '(height . 33))
(when window-system (set-frame-size (selected-frame) 83 36))

;; zoom in/out: globally change text size
(defadvice text-scale-increase (around all-buffers (arg) activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    BACKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change backup directory
(setq backup-directory-alist `(("." . "~/.backups")))

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 6   ;; Number of newest versions to keep.
      kept-old-versions 2   ;; Number of oldest backups to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      ;; backup-by-copying t  ;; Copy all files, don't rename them.
      delete-by-moving-to-trash t
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    AUTO-REVERT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Refresh buffers when the underlying file has changed
;; useful when using git or Bibdesk
(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'org-mode-hook 'use-)
;; M-<left/right> org-do-promote/demote

(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))
(global-set-key (kbd "C-c a") 'org-agenda)

;; C-up and down to navigate
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-<up>") 'org-previous-visible-heading)
  (define-key org-mode-map (kbd "C-<down>") 'org-next-visible-heading)
  ;; Add other org commands
  )

;; open files folded by default
(setq org-startup-folded t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AucTeX : compiler en pdf par défaut

(setq TeX-PDF-mode t)

;; Other AucTeX configuration
;; see http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/

(setq TeX-auto-save t) ;; creates 'auto' subdirectory to
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

;; disable reference style menu
;; "C-c )" directly works without extra RET step
(setq reftex-ref-macro-prompt nil)

(defun cite-format-tilde ()
  "Add a tilde to the citation format if necessary."
  (setq reftex-cite-format "~\\cite{%l}")
  (when (member (preceding-char) '(?\ ?\t ?\n ?~))
    (setq reftex-cite-format "\\cite{%l}"))
  )

(defun reftex-citation-tilde ()
  "Modified version of reftex-citation that inserts a tilde if needed."
  (interactive)
  (cite-format-tilde)
  (reftex-citation)  
  )

;; reftex keybindings
(add-hook 'reftex-mode-hook
	  #'(lambda ()
	      (define-key reftex-mode-map (kbd "C-c l") 'reftex-label)
	      (define-key reftex-mode-map (kbd "C-c r") 'reftex-reference)
	      ;; modified reftex citation
	      (define-key reftex-mode-map (kbd "C-c c") 'reftex-citation-tilde)
	      ;; original reftex citation
	      ;; (define-key reftex-mode-map (kbd "C-c c") 'reftex-citation)
	      ))

;; automatically pair dollars
(add-hook 'LaTeX-mode-hook
	  #'(lambda ()
	      (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)
	      ))

;; automatically pair
;; (defvar latex-electric-pairs '(
;; 			       (?\\\{ . ?\\\})
;;     			       (?\\\[ . ?\\\])
;; 			       ) "Electric pairs for LaTeX.")
;; (defun latex-add-electric-pairs ()
;;   (setq-local electric-pair-pairs (append electric-pair-pairs latex-electric-pairs))
;;   (setq-local electric-pair-text-pairs electric-pair-pairs))
;; (add-hook 'LaTeX-mode-hook 'latex-add-electric-pairs)

;; set backslash (escape) character as part of the word in auctex
;; allows C-left to jump before it
;; Also, now backward-up-list identifies the braces (solves above issue)
;; try it
(defun pglpm/modify-LaTeX-mode-syntax-table ()
  (modify-syntax-entry (string-to-char TeX-esc)
                       "w"
                       LaTeX-mode-syntax-table))

(add-hook 'LaTeX-mode-hook #'pglpm/modify-LaTeX-mode-syntax-table)

;; do not ask for optional arguments
(setq TeX-insert-macro-default-style 'mandatory-args-only)

;; better DEL: deletes full \command
(defun my/latex-smart-backspace ()
  "Smart backspace: if point is just after a LaTeX command (\\command), delete the whole command."
  (interactive)
  (let ((end (point)))
    (when (and (not (bobp))
               (save-excursion
                 (backward-word)
                 (looking-at "\\\\[a-zA-Z]+")
                 (= (match-end 0) end)))
      ;; Delete whole \command
      (let ((start (match-beginning 0)))
        (delete-region start end)))
    ;; Fallback to regular delete-backward
    (when (eq (point) end)
      (delete-backward-char 1))))

;; Bind DEL in LaTeX-mode
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "DEL") #'my/latex-smart-backspace))


;; Add backends provided by company-auctex to company-backends
(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init)
  ;; do not add $...$ when inserting a math symbol in text mode
  (setq company-auctex-symbol-math-symbol nil)
  (defun company-auctex-snippet-arg (arg)
    (let* ((opt (vectorp arg))
           (item (if opt (elt arg 0) arg))
           (var (format "${%s}" item)))
      (if opt
          ;; (concat "${[" var "]}") ;; original code
	  nil ;; instead: omit optional arguments (NB: nil = ())
	(concat "{" var "}")
	)
      ))
  )

;; todo-done
;; (require 'company-auctex)
;; (company-auctex-init)
;; ;; do not add $...$ when inserting a math symbol in text mode
;; (setq company-auctex-symbol-math-symbol nil)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
				:help "Run latexmk on file")
			      TeX-command-list)))
(add-hook 'TeX-mode-hook #'(lambda () (setq TeX-command-default "latexmk")))

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
      '(("lemma" ?l "lem:" "~\\ref{%s}" t ("lemma" "lemme" "lemmas"))
      	("theorem" ?t "thm:" "~\\ref{%s}" t ("theorem" "théorème" "theorems") -3)
      	("corollary" ?c "cor:" "~\\ref{%s}" t ("corollary" "corollaire") -2)
      	("proposition" ?p "prop:" "~\\ref{%s}" t ("proposition" "propositions"))
	("fact" ?f "fac:" "~\\ref{%s}" nil ("fact" "facts" "fait"))
      	("definition" ?d "def:" "~\\ref{%s}" t ("definition" "définition"))
	("remark" ?r "rem:" "~\\ref{%s}" nil ("remark" "remarque"))
	("example" ?x "ex:" "~\\ref{%s}" nil ("example" "exemple"))
	("assumption" ?a "ass:"  "~\\ref{%s}" nil ("assumption" "assumptions"))
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
	("\\\\cite" ispell-tex-arg-end)
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

(use-package outline-magic
  :ensure t
  :defer t
  )

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

;; modify current environment
;; C-u C-c C-e

;; Insert new entry to the cv bibliography
(defun cvpub ()
  "Add a new publication."
  (interactive) ;; function is a command, can be called by M-x
  (insert "\n% ================\n\\cvpub%\n"
	  "[" (read-string "STATUS: preprint | major | minor | accepted | published: [preprint] " nil nil "preprint") "]% status\n"
	  ;; ido-completing-read?
	  "{" (read-string "Year: [2024] " nil nil "2024") "}% year\n"
	  "{" (read-string "Title: ") "}% title\n"
	  "{" (read-string "Authors: [J. Mourtada] " nil nil "J. Mourtada") "}% authors\n"
	  "{" (read-string "arXiv id: ") "}% arXiv id\n"
	  "{" (read-string "Venue [empty unless revision]: ") "}% venue\n"
	  "{" (read-string "Volume(number):pages [empty unless published]: ") "}% volume(number):pages\n"
	  "{" (read-string "Link [empty unless published]:") "}% URL link\n"
	  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    CITAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bibliography management

(use-package citar
  :ensure t
  :config
  (setq citar-indicators nil) ;; removes space on the left
  (setq citar-latex-prompt-for-cite-style nil) ;; always insert with 'cite'
  ;; advise citar-insert-citation to add ~ when needed
  (defun my/citar-insert-tilde-if-needed (&rest _args)
    "Insert ~ before citation in LaTeX if not already inside \\cite{}, and only after alphabetic characters."
    (when (and (derived-mode-p 'latex-mode)
               (fboundp 'citar-latex--macro-bounds)
               (not (citar-latex--macro-bounds))
               (not (bolp))
               (save-excursion
		 (backward-char)
		 (let ((char (char-after)))
                   (and (not (member char '(?\s ?~ ?\n)))
			(char-equal (char-syntax char) ?w))))) ;; 'w' = word constituent (includes letters)
      (insert "~")))
  (advice-add 'citar-insert-citation :before #'my/citar-insert-tilde-if-needed)
  :bind ("C-c q" . citar-insert-citation)
  :custom
  (citar-bibliography '("~/Dropbox/taf/biblio.bib"))  
  (citar-templates
   '((main . "${author:18%sn}   ${date year issued:4}   ${title:32}   ${journal booktitle publisher:20}") ;; %sn or %etal
     (suffix . "     ${=key= id:15}    ${tags keywords:*}")
     (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on ${author editor:%etal}, ${title}")))
  )

;; below: quick fix for some issue; remove when no longer needed
(setq bibtex-dialect 'bibtex)

;; (setq citar-templates
;;       '((main . "${title:37}  ${date year issued:4}  ${author:18%sn}  ${journal booktitle publisher:18}")
;; 	(suffix . "   ${=key= id:*}") ;; %sn or %etal
;; 	(preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
;; 	(note . "Notes on ${author editor:%etal}, ${title}")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    IMENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; code navigation
(global-set-key (kbd "C-c i") 'imenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SHELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure environment variables inside GUI Emacs in OS X are the same
;; as in the user's shell

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

;; todo-done
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use elpy for python
(use-package elpy
  :ensure t
  :defer t ;; defer elpy loading
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  )

;; todo-done
;; (package-initialize)
;; (elpy-enable)

;; below: no longer needed?
;; (with-eval-after-load 'python
;;   (defun python-shell-completion-native-try ()
;;     "Return non-nil if can trigger native completion."
;;     (let ((python-shell-completion-native-enable t)
;;           (python-shell-completion-native-output-timeout
;;            python-shell-completion-native-try-output-timeout))
;;       (python-shell-completion-native-get-completions
;;        (get-buffer-process (current-buffer))
;;        nil "_"))))

;; use ipython (instead of python) as the default shell
;; make sure ipython is installed
;; (elpy-use-ipython) ; doesn't work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  ;; (setq markdown-coding-system "utf-8")
  (setq markdown-xhtml-header-content
	(concat "<script type=\"text/javascript\" async"
		" src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/"
		"2.7.1/MathJax.js?config=TeX-MML-AM_CHTML\">"
		"</script>"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JEMDOC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use jemdoc-mode
(use-package jemdoc-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load company mode html backend
;; is it needed?
;; (use-package company-web
;;   :ensure t)
;; (require 'company-web-html)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    OCAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first, install opam on command line
;; then, install tuareg, merlin and merlin-company in *Packages*

;; Add opam emacs directory to your load-path by appending this to your .emacs:
(let ((opam-share (ignore-errors (car (process-lines "opam" "var"
						     "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    ;;; (autoload 'merlin-mode "merlin" nil t nil) ;; commented by me
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    ))
;; Take a look at https://github.com/ocaml/merlin for more information

;; (require 'merlin-company)
;; ;; should be enough to get merlin to work within company

;; (use-package tuareg
;;   :ensure t
;;   :mode "\\.ml\\'"
;;   )

;; (use-package merlin
;;   :ensure t
;;   :after tuareg
;;   :mode "\\.ml\\'"
;;   )

;; (use-package merlin-company
;;   :ensure t
;;   :after merlin
;;   :mode "\\.ml\\'"
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    CUSTOM 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only needed in Emacs 28, to show 'about' screen
(add-to-list 'image-types 'svg)

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
   '(corfu merlin merlin-company tuareg citar vertico-prescient prescient consult marginalia use-package orderless vertico counsel ivy swiper nlinum company with-editor powerline solarized-theme magit default-text-scale outline-magic yasnippet company-web company-auctex web-mode markdown-mode elpy exec-path-from-shell ido-sort-mtime ido-ubiquitous ido-vertical-mode s smex moe-theme hlinum company-statistics caml auctex aggressive-indent frame-fns frame-cmds outline-magic ido-completing-read+ jemdoc-mode))
 '(safe-local-variable-values
   '((ispell-local-dictionary . français)
     (ispell-local-dictionary . fr)
     (TeX-parse-self . t)
     (TeX-auto-save . t))))
;; also: waher-theme, yasnippet, warm-night-theme, gotham-theme,
;; zerodark-theme, zenburn-theme, popup
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
