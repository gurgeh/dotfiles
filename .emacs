(require 'package)
(add-to-list 'package-archives                                                  
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar package-list
  '(ahg helm elpy auto-complete ein evernote-mode flymake flymake-cursor highlight-indentation ido-ubiquitous idomenu iedit base16-theme))

; fetch the list of packages available 
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Theming
(load-theme 'base16-solarized t) ; The trendiest theme? Use base16-default for black bkg
(setq inhibit-splash-screen t) ; Splash screen? Meh.
(setq inhibit-startup-message t) ; No startup message
(tool-bar-mode 0) ; No ugly toolbar
(show-paren-mode 1)
(setq frame-title-format (list "%b - " invocation-name))

(require 'helm) ; Completion helper - successor to anything
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'auto-complete-config)
(ac-config-default)

(require 'iedit) ; C-; for sublime multi-cursor-edit

; Elpy - Python in Emaacs
; requires pip install elpy rope
(elpy-enable)
(elpy-clean-modeline)
(setq elpy-default-minor-modes '(eldoc-mode flymake-mode yas-minor-mode auto-complete-mode)) ; I don't like highlight-indentation-mode

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; IPython Notebook - M-x ein:<tab> for commands
; Requires a notebook server (run "ipython notebook")
(require 'ein) 
(setq ein:use-auto-complete t)

(require 'flymake-cursor) ; See flymake messages for cursor position
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(require 'ahg) ; Mercurial support

;C/C++ convenience
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c C-c") 'recompile)))

(setq compile-command "scons")

; Tramp
(setq tramp-default-method "ssh")

; Evernote
(require 'evernote-mode)
(setq evernote-username "fnedrik") ; optional: you can use this username as default.
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
(global-set-key "\C-cec" 'evernote-create-note-in-notebook)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note-in-notebook)
(global-set-key "\C-cep" 'evernote-post-region-in-notebook)
(global-set-key "\C-ceb" 'evernote-browser)
