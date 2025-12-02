;;save compulsively instead
(global-auto-revert-mode t)
(setq make-backup-files nil)

;;we hates splash
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;;we hates ui
(unless (memq window-system '(mac ns))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;we hates tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default typescript-indent-level 2)

;;we hates noise
(setq native-comp-async-report-warnings-errors 'silent)

;;packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-list '(arduino-mode
                     nyan-mode
                     web-mode
                     markdown-mode
                     yaml-mode
                     git-modes
                     magit
                     company
                     bundler
                     inf-ruby))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;magit
(when (package-installed-p 'magit)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

;;ediff
(when (package-installed-p 'ediff)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (defvar my-ediff-last-windows nil)
  (defun my-store-pre-ediff-winconfig ()
    (setq my-ediff-last-windows (current-window-configuration)))
  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows))
  (add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig))

;;nyan-mode
(when (package-installed-p 'nyan-mode)
  (nyan-mode 1))

;;arduino-mode
(when (package-installed-p 'arduino-mode)
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode)))

;;ruby-mode
(when (package-installed-p 'ruby-mode)
  (add-hook 'ruby-mode-hook 'flymake-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("Procfile" . yaml-mode))
  (add-to-list 'auto-mode-alist '("Procfile.dev" . yaml-mode)))

;;inf-ruby
(when (package-installed-p 'inf-ruby)
  (setq inf-ruby-console-environment "development")
  (defalias 'project-rails-console 'inf-ruby-console-auto))

;;css-mode
(when (package-installed-p 'css-mode)
  (setq css-indent-offset 2))

;;js-mode
(when (package-installed-p 'js)
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

;;web-mode
(when (package-installed-p 'web-mode)
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

;;markdown-mode
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;;company-mode
(when (package-installed-p 'company)
  (setq company-global-modes '(ruby-mode))
  (setq company-backends '((company-etags)))
  (global-company-mode))

;;xref
(when (package-installed-p 'xref)
  (setq xref-prompt-for-identifier t))

;;etags
(when (package-installed-p 'etags)
  (setq tags-revert-without-query t))

;;compile
(when (package-installed-p 'compile)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;;custom modes
(define-derived-mode rspec-mode
  compilation-mode "RSpec"
  "Major mode for rspec compilation.")

(define-derived-mode rails-server-mode
  compilation-mode "Rails Server"
  "Major mode for rails server compilation.")

(define-derived-mode vite-ruby-mode
  compilation-mode "Vite Ruby"
  "Major mode for vite ruby compilation.")

(define-derived-mode agent-chat-mode
  gfm-mode "Agent Chat"
  "Major mode for agent chat compilation."
  (setq-local buffer-read-only t))

;;custom defuns
(require 'project)

(defun project-find-references ()
  (interactive)
  (let ((tags-file-name (concat (file-name-as-directory (project-root (project-current t))) "TAGS")))
    (when (file-exists-p tags-file-name)
      (call-interactively #'xref-find-references))))

(defun project-find-definitions ()
  (interactive)
  (let ((tags-file-name (concat (file-name-as-directory (project-root (project-current t))) "TAGS")))
    (when (file-exists-p tags-file-name)
      (call-interactively #'xref-find-definitions))))

(defun project-agent-chat ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (prompt (read-string-from-buffer "How can I help you today?" ""))
        (tmp-file-name "tmp/prompt.md")
        (compilation-buffer-name-function 'project-prefixed-buffer-name))
    (unless (string-empty-p prompt)
      (with-temp-file tmp-file-name
        (insert prompt))
      (compile (format "cat %s | chat" tmp-file-name t) 'agent-chat-mode))))

(defun project-agent-completion-at-point ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (when (buffer-file-name)
      (save-buffer)
      (insert
       (shell-command-to-string
        (format "cat %s | fim %d"
                (shell-quote-argument (file-relative-name (buffer-file-name)))
                (1- (point)) t))))))

(defalias 'project-agent 'project-agent-chat)

(defun project-rspec ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function 'project-prefixed-buffer-name))
    (when (buffer-file-name)
      (compile
       (format "bundle exec rspec %s --order defined --fail-fast"
               (shell-quote-argument (file-relative-name (buffer-file-name)) t))
       'rspec-mode))))

(defun project-rspec-at-point ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function 'project-prefixed-buffer-name))
    (when (buffer-file-name)
      (compile
       (format "bundle exec rspec %s:%s --fail-fast"
               (shell-quote-argument (file-relative-name (buffer-file-name)))
               (line-number-at-pos) t)
       'rspec-mode))))

(defun project-rails-server ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function 'project-prefixed-buffer-name))
    (if (comint-check-proc (project-prefixed-buffer-name "rails-server"))
        (pop-to-buffer (project-prefixed-buffer-name "rails-server"))
      (compile "bundle exec rails s" 'rails-server-mode))))

(defun project-vite-ruby ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function 'project-prefixed-buffer-name))
    (if (comint-check-proc (project-prefixed-buffer-name "vite-ruby"))
        (pop-to-buffer (project-prefixed-buffer-name "vite-ruby"))
      (compile "bundle exec vite dev" 'vite-ruby-mode))))

;;key bindings
(global-set-key (kbd "C-x p TAB") 'project-agent-completion-at-point)
(global-set-key (kbd "C-x p a") 'project-agent-chat)

;;window systems
(when window-system
  ;;theme  
  (defun my-theme-hook ()
    (let ((mac-mode (plist-get (mac-application-state) :appearance)))
      (cond ((equal mac-mode "NSAppearanceNameAqua")
             (load-theme 'modus-operandi t))
            ((equal mac-mode "NSAppearanceNameDarkAqua")
             (load-theme 'modus-vivendi t)))))
  (add-hook 'after-init-hook #'my-theme-hook)
  (add-hook 'mac-effective-appearance-change-hook #'my-theme-hook)
  ;;window
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;;font
  (set-face-attribute 'default nil :font "SF Mono-16")
  ;;emacsclient
  (require 'git-commit)
  (server-start))
