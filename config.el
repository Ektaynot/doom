;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "İsmail Efe Top"
      user-mail-address "ismailefetop@gmail.com")

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

(setq doom-theme 'doom-nord)

(setq doom-font (font-spec :family "JetBrains Mono" :size 22))

(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

(setq confirm-kill-emacs nil)

(setq bookmark-search-delay 0.0)
(setq mouse-scroll-delay 0.0)

(setq auto-save-default nil)

(setq backup-inhibited t)

(setq confirm-kill-processes nil)

(setq doom-modeline-enable-word-count t)

(setq display-line-numbers-type nil)

(setq use-dialog-box nil)

(setq delete-by-moving-to-trash t)

(setq calendar-week-start-day 1)

(setq org-cite-global-bibliography '("/Users/ismailefetop/uni/citation/bib.bib"))
(setq bibtex-completion-bibliography '("/Users/ismailefetop/uni/citation/bib.bib"))
(setq citar-bibliography '("/Users/ismailefetop/uni/citation/bib.bib"))
(setq org-cite-csl-styles-dir "/Users/ismailefetop/uni/citation/styles/")

(setq org-image-actual-width nil)

;;(super-save-mode +1)

;; warn when opening files bigger than 200MB
(setq large-file-warning-threshold 200000000)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(when (memq system-type '(darwin))
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(setq olivetti-body-width 94)

(add-hook 'text-mode-hook 'olivetti-mode)

(when (memq system-type '(darwin))
  (set-fontset-font t nil "SF Pro Display" nil 'append))

(setq undo-limit 80000000)

(setq evil-want-fine-undo t)

(use-package! org-pandoc-import :after org)

(add-hook 'org-mode-hook #'valign-mode)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(define-key evil-normal-state-map "ç" 'ispell)
(define-key evil-normal-state-map "ö" 'ispell-word)
(define-key evil-normal-state-map "ş" 'efe/google-current-word)
(define-key evil-normal-state-map "Ş" 'efe/first-result-url)
(define-key evil-normal-state-map "ğ" 'efe/tureng-english)
(define-key evil-normal-state-map "ü" 'efe/tureng-turkish)
(define-key evil-normal-state-map "Ğ" 'efe/tureng-en-fr)
(define-key evil-normal-state-map "Ü" 'efe/tureng-fr-en)

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(setq pandoc-binary "/opt/homebrew/bin/pandoc")

(setq pandoc-data-dir "/Users/ismailefetop/.config/doom/etc/pandoc/")

(setq org-directory "/Users/ismailefetop/.orgs/org/")

(setq org-agenda-files '("/Users/ismailefetop/uni/current-course/" "/Users/ismailefetop/.orgs/org/" "/Users/ismailefetop/ideas/"))

(setq org-confirm-babel-evaluate nil)

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(defun efe/google-current-word ()
  ;; initially written by chatgpt but later modified by u/Aminumbra
  "Search the current word on Google using browse-url."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (browse-url (concat "https://www.google.com/search?q=" word))
      (message "No word found at point."))))

(defun efe/first-result-url ()
  ;; Written by ChatGPT
  "Get the first url from a google search."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (let ((output (shell-command-to-string (format "firstresult -w %s" word))))
          (message output))
      (message "No word found at point."))))

(defun efe/select-and-copy-file-path ()
  ;; Written by chatgpt
  "Copy the selected file's path."
  (interactive)
  (let ((file-path (read-file-name "Select a file: ")))
    (kill-new file-path)
    (message "Copied file path: %s" file-path)))

(defun efe/insert-html-blog-template ()
  ;; Written by ChatGPT
  "Inserts HTML_HEAD lines at the first empty line and html code at the end of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((empty-line (progn (re-search-forward "^$" nil t) (point))))
      (goto-char empty-line)
      (insert "\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"/templates/style.css\" />\n")
      (insert "#+HTML_HEAD: <meta name=\"theme-color\" content=\"#fffcf0\">\n")
      (insert "#+HTML_HEAD: <link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"/favicon/apple-touch-icon.png\">\n")
      (insert "#+HTML_HEAD: <link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"/favicon/favicon-32x32.png\">\n")
      (insert "#+HTML_HEAD: <link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"/favicon/favicon-16x16.png\">\n")
      (insert "#+HTML_HEAD: <link rel=\"manifest\" href=\"/favicon/site.webmanifest\">\n")))
  (goto-char (point-max))
  (insert "\n\n")
  (insert "#+BEGIN_EXPORT html\n")
  (insert "<div class=\"bottom-header\">\n")
  (insert "  <a class=\"bottom-header-link\" href=\"/\">Home</a>\n")
  (insert "  <a href=\"mailto:ismailefetop@gmail.com\" class=\"bottom-header-link\">Mail Me</a>\n")
  (insert "  <a class=\"bottom-header-link\" href=\"/feed.xml\" target=\"_blank\">RSS</a>\n")
  (insert "  <a class=\"bottom-header-link\" href=\"https://github.com/Ektaynot/ismailefe_org\" target=\"_blank\">Source</a>\n")
  (insert "</div>\n")
  (insert "<div class=\"firechickenwebring\">\n")
  (insert "  <a href=\"https://firechicken.club/efe/prev\">←</a>\n")
  (insert "  <a href=\"https://firechicken.club\">🔥⁠🐓</a>\n")
  (insert "  <a href=\"https://firechicken.club/efe/next\">→</a>\n")
  (insert "</div>\n")
  (insert "#+END_EXPORT\n"))

(defun efe/term2anki (file)
  "Turn org notes into csv files that anki can read, creating a new file."
  (interactive "FExport notes to: ")
  (let* ((regex (rx bol (in "+-") " " (group (1+ nonl)) ": " (group (1+ nonl))))
         (buf (find-file-noselect file))
         (output "")
         (new-file (concat file ".anki.csv")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (setq output (concat output (format "%s;%s\n" (match-string 1)
                                            (match-string 2)))))
      (with-temp-file new-file
        (insert output))
      (kill-buffer buf)
      (message "Export done. New file: %s" new-file))))

(defun efe/remove-leading-spaces ()
  ;; Written by ChatGPT
  "Remove leading spaces until the first non-space character of each line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (delete-region (point-at-bol) (point))
      (forward-line))))

(defun efe/tureng-turkish ()
  ;; Written by ChatGPT
  "Translate the word at point using tureng program."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (let ((output (shell-command-to-string (format "tureng -l t -t e -w %s" word))))
          (message output))
      (message "No word found at point."))))

(defun efe/tureng-english ()
  ;; Written by ChatGPT
  "Translate the word at point using tureng program."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (let ((output (shell-command-to-string (format "tureng -l e -t t -w %s" word))))
          (message output))
      (message "No word found at point."))))

(defun efe/tureng-en-fr ()
  ;; Written by ChatGPT
  "Translate the word at point using tureng program."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (let ((output (shell-command-to-string (format "tureng -l e -t f -w %s" word))))
          (message output))
      (message "No word found at point."))))

(defun efe/tureng-fr-en ()
  ;; Written by ChatGPT
  "Translate the word at point using tureng program."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (let ((output (shell-command-to-string (format "tureng -l f -t e -w %s" word))))
          (message output))
      (message "No word found at point."))))

(defun efe/open-in-vscode ()
  ;; Written by ChatGPT
  "Open the current file in Visual Studio Code."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (shell-command (format "code %s" (shell-quote-argument file-path)))
      (message "Buffer is not visiting a file"))))

(defun efe/open-buffer-as-vscode-project ()
  ;; Written by ChatGPT
  "Open the current buffer's file in Visual Studio Code with the project directory as the workspace."
  (interactive)
  (let ((file-path (buffer-file-name))
        (project-root (or (project-root (project-current)) ;; Detect project root dynamically
                          (locate-dominating-file default-directory ".git") ;; Fallback to git root
                          default-directory))) ;; Fallback to the current directory
    (if file-path
        (progn
          (shell-command (format "code --folder-uri %s --goto %s"
                                 (shell-quote-argument (expand-file-name project-root))
                                 (shell-quote-argument (expand-file-name file-path))))
          (message "Opened file %s in VSCode using project root %s" file-path project-root))
      (message "Buffer is not visiting a file"))))

(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))

(set-file-template! "\\.org$" :trigger "__orgtemplate.org" :mode 'org-mode)

(setq browse-url-mailto-function 'browse-url-generic)
(setq browse-url-generic-program "open")

(add-hook 'emacs-startup-hook 'openwith-mode)
(setq openwith-associations
      '(("\\.pdf\\'" "open" (file))
        ("\\.docx\\'" "open" (file))
        ("\\.psd\\'" "open" (file))
        ;;("\\.jpeg\\'" "open" (file))
        ;;("\\.jpg\\'" "open" (file))
        ;;("\\.png\\'" "open" (file))
        ("\\.pptx\\'" "open" (file))
        ("\\.ppt\\'" "open" (file))
        ("\\.epub\\'" "open" (file))
        ;; ("\\.svg\\'" "open" (file))
        ("\\.gif\\'" "open" (file))
        ))

(setq ispell-program-name "hunspell")
(setq ispell-hunspell-dict-paths-alist '(("en_US" "/Users/ismailefetop/.config/dict/en_US.aff")))
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(flyspell-mode 1)

(defun endless/org-ispell ()
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+HTML_HEAD:" . ">"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+HTML" . ">"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_export" . "^#\\+end_export")))
(add-hook 'org-mode-hook #'endless/org-ispell)

;; Requires the mac app Rectangle to function.
(defun rectangle-maximize ()
  "Execute a shell command when Emacs starts."
  (call-process-shell-command "open -g 'rectangle://execute-action?name=maximize'" nil 0))

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(when (memq system-type '(darwin))
  (add-hook 'emacs-startup-hook 'rectangle-maximize)
)

(defun er-auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)

(defadvice! fixed-doom-modeline-update-vcs-a (&rest _)
  :after #'doom-modeline-update-vcs
  (and doom-modeline--vcs
       (equal (alist-get 'text doom-modeline--vcs) "master")
       (setf (alist-get 'text doom-modeline--vcs) "")))


