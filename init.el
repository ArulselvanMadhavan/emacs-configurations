;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; Disable welcome screen

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Global Variables
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      create-lockfiles nil
      make-backup-files nil
      column-number-mode t
      scroll-error-top-bottom t
      show-paren-delay 0.5
      use-package-always-ensure t
      sentence-end-double-space nil)

;; Buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; Modes
(electric-indent-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Resize window
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)


;; Package Archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
package-archive-priorities '(("melpa-stable" . 1)))

;; Package Initialize
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package ensime
  :ensure t
  :pin melpa-stable)
;; Shortcut to convert horizontal window to vertical window split
;; https://stackoverflow.com/questions/14881020/emacs-shortcut-to-switch-from-a-horizontal-split-to-a-vertical-split-in-one-move
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; Save All on focus out
(defun save-all ()
    (interactive)
    (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; Winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

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

;; Projectile create new file if not found
(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
	"Create-file"
      :action 'find-file))
  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

;; Projectile ignores
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories "bower_components")
(add-to-list 'projectile-globally-ignored-directories ".git")

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
(setq auto-save-default t)
(setq auto-save-visited-file-name t)
(setq auto-save-timeout 1)

;; Set backup directory
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; Flycheck
;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

;; (add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line)
      flycheck-idle-change-delay 0.8)

;; Tern
(add-hook 'after-init-hook 'global-company-mode)

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

;; Setup TIDE for jsx mode
(require 'tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change)
	flycheck-idle-change-delay 0.8)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(add-hook 'rjsx-mode-hook #'setup-tide-mode)

;; Magit
(require 'magit)
(setq magit-view-git-manual-method 'man)

(global-set-key (kbd "C-c g m") 'magit-status)


;; via https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.el
(defun haskell-style-tibbe ()
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4
        haskell-indentation-where-pre-offset 4
        haskell-indentation-where-post-offset 4
        haskell-indentation-layout-offset 4))

;; ---------------------------------------------------------------------

;; Haskell Intero
(use-package intero
  :init
  (haskell-style-tibbe)
  (setq haskell-stylish-on-save t
	haskell-tags-on-save t)
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  )
;; (require 'intero)
;; (add-hook 'haskell-mode-hook (lambda()
;; 			       (intero-mode)
;; 			       (hindent-mode)
;; 			       ))

;; PDF tools install
(pdf-tools-install)

;; Avy
(use-package avy
  :ensure t
  :bind ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

;; Window management
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Ace-window
(use-package ace-window
          :ensure t
          :defer 1
          :config
	  (global-set-key (kbd "M-p") 'ace-window)
          (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
          (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
          (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
                aw-dispatch-always t
                aw-dispatch-alist
                '((?x aw-delete-window     "Ace - Delete Window")
                  (?c aw-swap-window       "Ace - Swap Window")
                  (?n aw-flip-window)
                  (?v aw-split-window-vert "Ace - Split Vert Window")
                  (?h aw-split-window-horz "Ace - Split Horz Window")
                  (?m delete-other-windows "Ace - Maximize Window")
                  (?g delete-other-windows)
                  (?b balance-windows)
                  (?u winner-undo)
                  (?r winner-redo)))
	  )

;; Auto pair for balancing parenthesis
(require 'autopair)
(autopair-global-mode)

;; Select mini buffer
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;; Undo tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))
;; Bind to C-c o
(global-set-key "\C-co" 'switch-to-minibuffer)
;; Code folding - Origami
(require 'origami)
(global-origami-mode)

(add-hook 'origami-mode-hook
          (lambda()
             ;; (add-to-list 'origami-parser-alist '(typescript-mode . origami-c-style-parser))
            (define-key origami-mode-map (kbd "C-c C-c f") 'origami-recursively-toggle-node)
            (define-key origami-mode-map (kbd "C-c C-c a") 'origami-toggle-all-nodes)
            (define-key origami-mode-map (kbd "C-c C-c s") 'origami-show-only-node)
            (define-key origami-mode-map (kbd "C-c C-c o") 'origami-open-node)
            (define-key origami-mode-map (kbd "C-c C-c p") 'origami-open-node-recursively)
            (define-key origami-mode-map (kbd "C-c C-c k") 'origami-close-node-recursively)
            (define-key origami-mode-map (kbd "C-c C-c c") 'origami-close-node)))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ([f8]         . treemacs-toggle)
        ("M-0"        . treemacs-select-window)
        ("C-c 1"      . treemacs-delete-other-windows)
        ("M-t"        . nil)            ;Unset the key to use treemacs
        ("M-t ft"     . treemacs-toggle)
        ("M-t fT"     . treemacs)
        ("M-t fB"     . treemacs-bookmark)
        ("M-t f C-t"  . treemacs-find-file)
        ("M-t f M-t" . treemacs-find-tag)))
(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("M-t fP" . treemacs-projectile)
              ("M-t fp" . treemacs-projectile-toggle)))
;; NVM
;; (require-package 'nvm)

;; (defun do-nvm-use (version)
;;   (interactive "sVersion:")
;;   (nvm-use version)
;;   (exec-path-from-shell-copy-env "PATH"))

;; (defun run-node (cwd)
;;   (interactive "DDirectory: ")
;;   (unless (executable-find "node")
;;     (call-interactively 'do-nvm-use))
;;   (let ((default-directory cwd))
;;     (pop-to-buffer (make-comint (format "node-repl-%s" cwd) "node" nil "--interactive"))))


;; Haskell stylish mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2f0a552a9d14fe8ddaaacdb7b82a0eee1ea1f7f5d0850789915e5b04a1b9669f" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "6952b5d43bbd4f1c6727ff61bc9bf5677d385e101433b78ada9c3f0e3787af06" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "0b6cb9b19138f9a859ad1b7f753958d8a36a464c6d10550119b2838cedf92171" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e7aa1c216e14ec250a367908e305dc7f2d549a4b3f87f4a0515ef5d3800fc2f5" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "b378249b7f647796b186c70f61eaaee7aa1bd123681d5ca8c44d3ca8875e1b70" "77ab33a45a8d39566f24cd6a9b28c78eec89882004ed194587e210399813d009" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(mac-command-modifier (quote super))
 '(mac-mouse-wheel-mode nil)
 '(mac-option-modifier (quote meta))
 '(package-selected-packages
   (quote
    (treemacs-evil treemacs-projectile real-auto-save origami undo-tree treemacs ensime web-mode json-mode nvm hindent autopair intero darktooth-theme pdf-tools haskell-mode tide magit tern-auto-complete prettier-js company-tern flycheck twilight-bright-theme twilight-anti-bright-theme twilight-theme rjsx-mode helm-projectile monokai-theme moe-theme grandshell-theme ample-theme solarized-theme zenburn-theme xref-js2 which-key use-package try org-bullets js2-refactor helm doom-themes cyberpunk-theme counsel color-theme-sanityinc-tomorrow color-theme auto-complete ace-window))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



