;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; Disable welcome screen

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; IDO
;; (ido-mode 1)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)

;; Winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Add line numbers
(global-linum-mode 1)

;; Package Archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa-stable" .  "https://stable-melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

;;Load Theme
(add-hook 'after-init-hook
	  (lambda() (load-theme 'twilight-bright t)))
;; (setq doom-themes-enable-bold t
      ;; doom-themes-enable-italic t)

;; Key Bindings
;; Bind iMenu
(global-set-key (kbd "M-i") 'imenu)

;; Helm
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;rebind-tab-to-run-persistent-action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ;make TAB work in terminal
(define-key helm-map (kbd "C-b") 'helm-select-action) ;list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line        t
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)
(helm-mode 1)

;; Helm Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; JS2-Mode
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))

;; Better iMenu for js2 mode
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; JS2-refactor and xref-js2
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

;; js-mode binds "M-." which conflicts with xref, so unbind it
(define-key js2-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; Map kill command in js2
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; Sort apropos result by relevancy
(setq apropos-sort-by-scores t)

;; Set the title of the buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Turn off auto-save
(setq auto-save-default nil)

;; Set backup directory
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Tern
(add-hook 'after-init-hook 'global-company-mode)
;; (require 'company-tern)
;; (add-to-list 'company-backends 'company-tern)
;; (add-hook 'rjsx-mode-hook (lambda()
;; 			    (tern-mode t)
;; 			    (company-mode t)))

;; Sync environment variables between emacs and shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Select eslint config from node_modules
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; Prettier Configuration
(defun my/use-prettier-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
                      (expand-file-name "node_modules/prettier/bin/prettier.js"
                                        root))))
    (when (and prettier (file-executable-p prettier))
      (setq-local prettier-js-command prettier))))

(add-hook 'rjsx-mode-hook #'my/use-prettier-from-node-modules)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "6952b5d43bbd4f1c6727ff61bc9bf5677d385e101433b78ada9c3f0e3787af06" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "0b6cb9b19138f9a859ad1b7f753958d8a36a464c6d10550119b2838cedf92171" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e7aa1c216e14ec250a367908e305dc7f2d549a4b3f87f4a0515ef5d3800fc2f5" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "b378249b7f647796b186c70f61eaaee7aa1bd123681d5ca8c44d3ca8875e1b70" "77ab33a45a8d39566f24cd6a9b28c78eec89882004ed194587e210399813d009" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(mac-command-modifier (quote super))
 '(mac-mouse-wheel-mode nil)
 '(mac-option-modifier (quote meta))
 '(package-selected-packages
   (quote
    (magit tern-auto-complete prettier-js company-tern flycheck twilight-bright-theme twilight-anti-bright-theme twilight-theme rjsx-mode helm-projectile monokai-theme moe-theme grandshell-theme ample-theme solarized-theme zenburn-theme xref-js2 which-key use-package try org-bullets js2-refactor helm doom-themes cyberpunk-theme counsel color-theme-sanityinc-tomorrow color-theme auto-complete ace-window))))
