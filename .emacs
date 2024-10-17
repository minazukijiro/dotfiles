(setq default-directory "~/")

(add-hook
 'after-save-hook
 (lambda ()
   (let ((file-name (buffer-file-name (current-buffer))))
     (when (and (file-exists-p file-name)
                (eq (point-min) (point-max)))
       (delete-file file-name)))))

(add-hook
 'after-save-hook
 'executable-make-buffer-file-executable-if-script-p)

(add-hook
 'kill-emacs-hook
 (lambda ()
   (when (and
          (boundp 'custom-file)
          (file-exists-p custom-file))
     (delete-file custom-file))))

(defvar scratch-buffer-file
  (locate-user-emacs-file "scratch"))

(add-hook
 'after-init-hook
 (lambda ()
   (when (file-exists-p scratch-buffer-file)
     (with-current-buffer (get-buffer-create "*scratch*")
       (erase-buffer)
       (insert-file-contents scratch-buffer-file)))))

(add-hook
 'kill-emacs-hook
 (lambda ()
   (with-current-buffer (get-buffer-create "*scratch*")
     (write-region (point-min) (point-max) scratch-buffer-file nil t))))

(add-hook
 'buffer-kill-hook
 (lambda ()
   (when (eq (current-buffer) (get-buffer "*scratch*"))
     (rename-buffer "*scratch~*")
     (clone-buffer "*scratch*"))))

(add-hook
 'after-init-hook
 (lambda ()
   (message "theme setting start")
   (custom-set-variables
    '(custom-safe-themes t)
    '(mode-line-format nil)
    '(menu-bar-mode nil)
    '(tool-bar-mode nil)
    '(scroll-bar-mode nil)
    '(header-line-format
      '("%l,%C  "
        (:eval
         (cond
          ((not (buffer-file-name))
           (buffer-name))
          (buffer-read-only
           (propertize buffer-file-truename 'face 'italic))
          ((buffer-modified-p)
           (propertize buffer-file-truename 'face 'bold))
          (t
           buffer-file-truename)))
        skk-modeline-input-mode
        (:eval
         (propertize " " 'display `(space :align-to (- right ,(length mode-name)))))
        mode-name)))
   (load-theme 'anticolor t)
   (message "theme setting start")))

(custom-set-variables
 '(custom-file (locate-user-emacs-file (format "emacs-%d.el" (emacs-pid))))
 '(exec-path
   `(,(expand-file-name "~/bin")
     ,(expand-file-name "~/.local/share/mise/shims")
     "/usr/local/bin"
     "/usr/bin"
     "/bin"
     "/usr/sbin"
     "/sbin"))
 '(find-file-visit-truename t)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(make-backup-files nil)
 '(pop-up-windows nil)
 '(require-final-newline 'visit-save)
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(set-file-name-coding-system 'utf-8)
 '(set-keyboard-coding-system 'utf-8)
 '(set-mark-command-repeat-pop t)
 '(set-terminal-coding-system 'utf-8)
 '(show-paren-mode t)
 '(split-width-threshold 0)
 '(system-time-locale "C" t))

(let ((display-table (or buffer-display-table standard-display-table)))
  (when display-table
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Tables.html
    (set-display-table-slot display-table 1 ? )
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(eval-and-compile
  (customize-set-variable
   'package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

  (package-initialize)

  (unless (package-installed-p 'leaf)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    ;; (leaf-keywords-init)))
    ))

(leaf eshell
  :bind ("C-c #" . eshell)
  :custom (eshell-path-env . `,(string-join exec-path ":"))
  :config
  (defun eshell/hello ()
    (message "hello world")))

(leaf ido
  :custom
  (ido-enable-flex-matching . t)
  (ido-use-faces . t)
  :config
  (leaf imenu-anywhere :ensure t :bind ("M-." . ido-imenu-anywhere))
  (leaf smex :ensure t :bind (("M-x" . smex) ("M-X" . smex-major-mode-commands)))
  (leaf ido-vertical-mode
    :ensure t
    :custom (ido-vertical-define-keys . 'C-n-and-C-p-only)
    :global-minor-mode ido-vertical-mode)
  (leaf ido-completing-read+ :ensure t :global-minor-mode ido-ubiquitous-mode)
  (ido-mode t)
  (ido-everywhere t))

(leaf whitespace
  :hook
  (before-save-hook . whitespace-cleanup)
  :custom
  (whitespace-space-regexp . "\\(\u3000+\\)")
  (whitespace-style . '(face trailing spaces empty space-mark tab-mark))
  (whitespace-display-mappings . '((space-mark ?\u3000 [?\u25a1]) (tab-mark ?\t [?\u00bb ?\t] [?\\ ?\t])))
  (whitespace-action . '(auto-cleanup))
  :global-minor-mode global-whitespace-mode)

(leaf company
  :ensure t
  :bind ("C-c i" . company-complete)
  :custom
  (company-files . t)
  (company-idle-delay . nil)
  (company-section-wrap-around . t)
  :config
  (leaf company-statistics :ensure t :global-minor-mode company-statistics-mode)
  (leaf company-c-headers :ensure t :config (add-to-list 'company-backends 'company-c-headers))
  (leaf company-shell :ensure t :config (add-to-list 'company-backends 'company-shell))
  (leaf company-terraform :ensure t :config (add-to-list 'company-backends 'company-terraform))
  (leaf company-go :ensure t :config (add-to-list 'company-backends 'company-go))
  (leaf company-nginx :ensure t :config (add-to-list 'company-backends 'company-nginx))
  :global-minor-mode global-company-mode)

(leaf ddskk
  :ensure t
  :custom
  (default-input-method . "japanese-skk")
  (skk-status-indicator . 'minor-mode)
  (skk-egg-like-newline . t)
  (skk-latin-mode-string . "a")
  (skk-hiragana-mode-string . "あ")
  (skk-katakana-mode-string . "ア")
  (skk-jisx0208-latin-mode-string . "Ａ")
  :config
  (let ((jisyo (locate-user-emacs-file "jisyo")))
    (unless (file-directory-p jisyo)
      (skk-get jisyo))))

(leaf dockerfile-mode :ensure t)

(leaf editorconfig :ensure t)

(leaf find-file
  :config
  (setq default-directory "~/"))

(leaf flycheck
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom
  (flycheck-display-errors-delay . 0.3)
  :config
  (leaf flycheck-inline :ensure t :hook (flycheck-mode-hook . flycheck-inline-mode))
  :global-minor-mode global-flycheck-mode)

(leaf folding :ensure t)

(leaf js-mode
  :hook (js-mode-hook . (lambda ()
                          (make-local-variable 'js-indent-level)
                          (setq js-indent-level 2))))

(leaf k8s-mode :ensure t)

(leaf lua-mode :ensure t)

(leaf macrostep :ensure t :bind ("C-c e" . macrostep-expand))

(leaf magit :ensure t)

(leaf markdown-mode :ensure t)

(leaf open-junk-file
  :preface
  (defun my:open-junk-file-delete-files ()
    (when (and
           (boundp 'my:open-junk-file-directory)
           (file-directory-p my:open-junk-file-directory))
      (dolist (x
               (directory-files my:open-junk-file-directory t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
        (delete-file x))))
  :ensure t
  :bind ("C-c j" . open-junk-file)
  :hook (kill-emacs-hook . my:open-junk-file-delete-files)
  :init
  (setq my:open-junk-file-directory (locate-user-emacs-file "junk/"))
  (setq open-junk-file-format (concat my:open-junk-file-directory "%s.")))

(leaf org-mode
  :bind ("C-c c" . org-capture))

(leaf popwin
  :ensure t
  :config
  (mapcar
   #'(lambda (x) (push x popwin:special-display-config))
   '(("*Buffer List*")
     ("*eshell*" :height 30 :dedicated t :stick t)
     ("*Warnings*")))
  :global-minor-mode popwin-mode)

(leaf rainbow-mode :ensure t)

(leaf terraform-mode
  :ensure t
  :hook (terraform-mode-hook . terraform-format-on-save-mode)
  :config
  (leaf terraform-doc :ensure t))

(leaf typescript-mode :ensure t)

(leaf xclip
  :if (or (executable-find "xclip")
          (executable-find "xsel")
          (executable-find "pbcopy"))
  :ensure t
  :global-minor-mode xclip-mode)

(leaf yaml-mode :ensure t)
