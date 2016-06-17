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
;;;;    LOAD PACKAGES
;;;;    THEMES
;;;;    ELPA, MELPA
;;;;    KEY BINDINGS
;;;;    PARENTHESES
;;;;    SCROLLING CUSTOMIZATION
;;;;    SPELL CHECKING
;;;;    LINE NUMBERING
;;;;    GRAPHICAL USER INTERFACE
;;;;    FONTS
;;;;    POWERLINE
;;;;    FRAME SIZE
;;;;    BACKUP
;;;;    AUCTEX
;;;;    SHELL
;;;;    AUTOCOMPLETE
;;;;    CUSTOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LOAD PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell emacs where your personal elisp library directory is :
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

;; (load "minimap") ;; M-x minimap-mode pour activer la minimap.
;; Ajouter un hook (longueur <- 108) lorsque l'on enclenche le minimap-mode.

(when (>= emacs-major-version 24)
  (require 'ido-vertical-mode)
  (ido-mode 1)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

;; Auto-start Smex every time you open Emacs.
(when (>= emacs-major-version 24)
  (load "smex")
  (require 'smex) ;; Not needed if you use package.el
  (smex-initialize) ;; Can be omitted. This might cause a (minimal) delay
                    ;; when Smex is auto-initialized on its first run.
  )

(load "lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

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
;;;;    THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Thèmes
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized"))

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
;;;;    KEY BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mes keybindings persos (peuvent être changés si besoin est)

;; (global-set-key (quote [s-up]) (quote beginning-of-buffer))
;; (global-set-key (quote [s-down]) (quote end-of-buffer))

;; (add-hook 'TeX-mode-hook 'my-make-slash-backslash)

;; Special characters
(global-set-key (kbd "M-/") "\\")
(global-set-key (kbd "M-L") "|")
(global-set-key (kbd "M-n") "~")

(global-set-key (kbd "M-(") "{")
(global-set-key (kbd "M-)") "}")

(global-set-key (kbd "M-5") "[") ;; peut poser problème, car cela supprime
                                 ;; M-5 (qui revient +/- à C-u 5)
(global-set-key (kbd "M-°") "]")   
;; (global-set-key (kbd "C-M-)") "]") ;; alternative

(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "M-<up>") 'backward-paragraph)

;; Change window (from Wind Move built-in library) : Cmd + arrow
(windmove-default-keybindings 'super)

;; Window resize : Cmd + ctrl + arrow
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)
(global-set-key (kbd "C-s-=") 'balance-windows)

;; Change frame
(global-set-key (kbd "s-<") 'other-frame)
;; (global-set-key (kbd "s->") 'other-frame)

;; Frame movement (from frame-cmds.el) : Cmd + fn + arrow
(global-set-key (kbd "s-<prior>") 'move-frame-up)
(global-set-key (kbd "s-<next>") 'move-frame-down)
(global-set-key (kbd "s-<home>") 'move-frame-left)
(global-set-key (kbd "s-<end>") 'move-frame-right)

;; Frame resize (from frame-cmds.el) : Cmd + alt + arrow
(global-set-key (kbd "M-s-<down>")  'enlarge-frame)
(global-set-key (kbd "M-s-<right>") 'enlarge-frame-horizontally)
(global-set-key (kbd "M-s-<up>")    'shrink-frame)
(global-set-key (kbd "M-s-<left>")  'shrink-frame-horizontally)

;; Commands to minimize/maximize/fullscreen
(global-set-key (kbd "C-x -") 'suspend-frame) ;; same as C-z
(global-set-key (kbd "C-x +") 'toggle-frame-maximized) ;; same as M-f10
(global-set-key (kbd "C-x _") 'toggle-frame-fullscreen) ;; same as M-f11
(global-set-key (kbd "<M-f11>") 'toggle-frame-fullscreen)

;; Zoom in/out
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Use smex by default on Emacs >= 24
(when (>= emacs-major-version 24)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c M-x") 'execute-extended-command))

;; C-x C-f now uses ido-find-file.
;; To leave ido, use C-f (file), C-b (buffer), C-d (dired)
;; Useful to create a new file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PARENTHESES
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SPELL CHECKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "DICTIONARY" "fr")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    LINE NUMBERING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system (global-linum-mode t) (setq linum-format "%3d"))
;; (setq linum-format "%4d \u2502")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    GRAPHICAL USER INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(scroll-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    FONTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun use-djvumono ()
  "Switch the current buffer to a DejaVu Sans Mono font."
  (set-frame-font "DejaVu Sans Mono-13"))

(add-hook 'tuareg-mode-hook 'use-djvumono)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    POWERLINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
(require 'powerline)
(powerline-default-theme)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    FRAME SIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system (add-to-list 'default-frame-alist '(width . 83)))
;; (add-to-list 'default-frame-alist '(height . 33))
;; (when window-system (set-frame-size (selected-frame) 83 43))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    BACKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change backup directory
(setq backup-directory-alist `(("." . "~/.backups")))

(setq delete-old-versions t)  ;; delete excess backup files silently

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AucTeX : compiler en pdf par défaut

(setq TeX-PDF-mode t)

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
  (setq company-idle-delay 0))

;; (require 'auto-complete)
;; (global-auto-complete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    CUSTOM 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (zenburn)))
 ;; either adwaita or misterioso or zenburn or gotham or wombat
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "590759adc4a5bf7a183df81654cce13b96089e026af67d92b5eec658fb3fe22f" "357d5abe6f693f2875bb3113f5c031b7031f21717e8078f90d9d9bc3a14bcbd8" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(doc-view-continuous t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
