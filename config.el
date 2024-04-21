;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "İsmail Efe Top"
      user-mail-address "ismailefetop@gmail.com")

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

(setq doom-theme 'kanagawa)

(setq doom-font (font-spec :family "JetBrains Mono" :size 22))

(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

(setq confirm-kill-emacs nil)

(setq which-key-idle-delay 0.0)
(setq bookmark-search-delay 0.0)
(setq mouse-scroll-delay 0.0)

;disable backup
 (setq backup-inhibited t)
;disable auto save
 (setq auto-save-default nil)

(setq confirm-kill-processes nil)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq doom-modeline-enable-word-count t)

(setq display-line-numbers-type nil)

(setq use-dialog-box nil)'

(setq calendar-week-start-day 1)

(setq olivetti-body-width 94)

(setq org-cite-global-bibliography '("/Users/ismailefetop/uni/citation/bib.bib"))
(setq! bibtex-completion-bibliography '("/Users/ismailefetop/uni/citation/bib.bib"))
(setq! citar-bibliography '("/Users/ismailefetop/uni/citation/bib.bib"))
(setq org-cite-csl-styles-dir "/Users/ismailefetop/uni/citation/styles/")

(super-save-mode +1)

;; warn when opening files bigger than 200MB
(setq large-file-warning-threshold 200000000)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(add-hook 'text-mode-hook 'olivetti-mode)

(define-key evil-normal-state-map "ç" 'ispell)
(define-key evil-normal-state-map "ö" 'ispell-word)
(define-key evil-normal-state-map "ş" 'google-current-word)
(define-key evil-normal-state-map "ğ" 'efe/tureng-english)
(define-key evil-normal-state-map "ü" 'efe/tureng-turkish)

(setq pandoc-binary "/opt/homebrew/bin/pandoc")

(setq pandoc-data-dir "/Users/ismailefetop/.config/doom/etc/pandoc/")

(setq org-directory "~/.orgs/org/")

(setq org-agenda-files '("~/uni/current-course/" "~/.orgs/org/" "~/ideas/"))

(setq org-confirm-babel-evaluate nil)

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(after! org
  :config
  ;; to start the agende from the current day
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-day "+0d")
  ;; set span 7
  (setq org-agenda-span 7)
  ;; Add additional configuration here
  )

(after! org
  (setq org-capture-templates
        ;; Below lines are for school captures
        '(("t" "School Todo" entry (file+olp+datetree "~/uni/current-course/todo.org")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Class Note" entry (file+olp+datetree "~/uni/current-course/notes/%A.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ;; Below lines are for org-chef
          ("c" "Cookbook" entry (file "~/ideas/recipes/cookbook.org")
           "%(org-chef-get-recipe-from-url)"
           :empty-lines 1)
          ("m" "Manual Cookbook" entry (file "~/ideas/recipes/cookbook.org")
           "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n"))))

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

;; below code is fixed by u/Aminumbra
(defun google-current-word ()
  "Search the current word on Google using browse-url."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (browse-url (concat "https://www.google.com/search?q=" word))
      (message "No word found at point."))))

(defun open-finder-and-copy-path ()
  "Open Finder and copy the selected file's path."
  (interactive)
  (let ((file-path (read-file-name "Select a file: ")))
    (kill-new file-path)
    (message "Copied file path: %s" file-path)
    (start-process "finder" nil "open" "-R" file-path)))
(defun close-all-buffers ()
(interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun efe/reading-mode ()
  "Toggle reading mode."
  (interactive)
  (hide-mode-line-mode +1)
  ;; (load-theme 'kanagawa)
  (olivetti-mode)
  ;; (setq hl-line-mode nil)
  (menu-bar--display-line-numbers-mode-none))
(global-set-key (kbd "C-ö") 'efe/reading-mode)

(defun efe/undo-reading-mode ()
  "undo reading mode."
  (interactive)
  ;; (disable-theme 'kanagawa)

  ;; (load-theme 'doom-dracula t)

  (hide-mode-line-mode -1)
  (setq olivetti-mode nil)
  ;; (setq hl-line-mode t)
  (menu-bar--display-line-numbers-mode-absolute))
(global-set-key (kbd "C-ç") 'efe/undo-reading-mode)

(defun efe/export-to-docx ()
  "Output to docx using pandoc-mode"
  (interactive)
  (pandoc-mode)
  (execute-kbd-macro (kbd "C-c / O W d b b r"))
  (setq pandoc-mode nil)
  )

(defun efe/insert-html-blog-template ()
  "Inserts HTML_HEAD lines at the first empty line and html code at the end of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((empty-line (progn (re-search-forward "^$" nil t) (point))))
      (goto-char empty-line)
      (insert "\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"/templates/style.css\" />\n")
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
  (interactive "FExport notes to: ")
  (let ((regex (rx bol (in "+-") " " (group (1+ nonl)) ": " (group (1+ nonl))))
        (buf (find-file-noselect file))
        (output ""))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (setq output (concat output (format "%s;%s\n" (match-string 1)
                                            (match-string 2)))))
      (with-current-buffer buf
        (erase-buffer)
        (insert output)
        (save-buffer))
      (kill-buffer buf)
      (message "Export done."))))

(defun remove-leading-spaces ()
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
  "Translate the word at point using tureng program."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (let ((output (shell-command-to-string (format "tureng -l t -w %s" word))))
          (message output))
      (message "No word found at point."))))

(defun efe/tureng-english ()
  "Translate the word at point using tureng program."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (let ((output (shell-command-to-string (format "tureng -l e -w %s" word))))
          (message output))
      (message "No word found at point."))))

(defun efe/insert-elisp-src-block ()
  "Inserts a two-line emacs lisp source block."
  (interactive)
  (insert "\n#+begin_src elisp\n\n")
  (save-excursion
    (insert "#+end_src\n")))

(set-file-template! "\\.org$" :trigger "__orgtemplate.org" :mode 'org-mode)

(setq browse-url-mailto-function 'browse-url-generic)
(setq browse-url-generic-program "open")

(openwith-mode t)
(setq openwith-associations
      '(("\\.pdf\\'" "open" (file))
        ("\\.docx\\'" "open" (file))
        ("\\.psd\\'" "open" (file))
        ;;("\\.jpeg\\'" "open" (file))
        ;;("\\.jpg\\'" "open" (file))
        ;;("\\.png\\'" "open" (file))
        ("\\.pptx\\'" "open" (file))
        ("\\.epub\\'" "open" (file))
        ;; ("\\.svg\\'" "open" (file))
        ("\\.gif\\'" "open" (file))
        ;; Add more image formats as needed
        ))

(setq ispell-program-name "hunspell")
(setq ispell-hunspell-dict-paths-alist '(("en_US" "/Users/ismailefetop/.config/dict/en_US.aff")))
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(flyspell-mode 1)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(after! gcmh
  (setq gcmh-high-cons-threshold (* 64 1048576)))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq set-message-beep 'silent)

(setq org-image-actual-width nil)

;; Requires the mac app Rectangle to function.
(defun rectangle-maximize ()
  "Execute a shell command when Emacs starts."
  (call-process-shell-command "open -g 'rectangle://execute-action?name=maximize'" nil 0))

;; Add the function to the Emacs startup hook
(add-hook 'emacs-startup-hook 'rectangle-maximize)

 (use-package ox-pandoc
        :after ox)
;; default options for all output formats
(setq org-pandoc-options '((standalone . t)))
;; cancel above settings only for 'docx' format
(setq org-pandoc-options-for-docx '((standalone . nil)))
;; special settings for beamer-pdf and latex-pdf exporters
(setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
;; special extensions for markdown_github output
(setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))
