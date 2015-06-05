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
  '(ahg helm elpy auto-complete ein flymake flymake-cursor highlight-indentation ido-ubiquitous idomenu iedit base16-theme py-autopep8))

; fetch the list of packages available 
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Theming
(load-theme 'base16-solarized-dark t) ; The trendiest theme? Use base16-default for black bkg
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

; Elpy - Python in Emacs
; requires pip install elpy rope
(elpy-enable)
;(elpy-clean-modeline)
(setq elpy-default-minor-modes '(eldoc-mode flymake-mode yas-minor-mode auto-complete-mode))

(add-hook 'python-mode-hook (highlight-indentation-mode 0))  ; I don't like highlight-indentation-mode


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default column-number-mode 1)

; requires pip install autopep8
(require 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=100"))
(add-hook 'python-mode-hook
  (lambda() 
    (local-set-key  (kbd "C-c p") 'py-autopep8))) ; Use C-c p to apply pep8 formatting automatically

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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#6c71c4" "#268bd2" "#93a1a1"])
 '(ansi-term-color-vector
   [unspecified "#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#6c71c4" "#268bd2" "#93a1a1"])
 '(custom-safe-themes
   (quote
    ("6ebb2401451dc6d01cd761eef8fe24812a57793c5ccc427b600893fa1d767b1d" default))))
