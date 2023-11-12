(setq user-full-name "İsmail Efe Top"
      user-mail-address "ismailefetop@gmail.com")

(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "JetBrains Mono" :size 22))

(setq display-line-numbers-type t)

(setq confirm-kill-emacs nil)

;disable backup
 (setq backup-inhibited t)
;disable auto save
 (setq auto-save-default nil)

(setq confirm-kill-processes nil)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq calendar-week-start-day 1)

(setq olivetti-body-width 100)

(super-save-mode +1)

;; warn when opening files bigger than 200MB
(setq large-file-warning-threshold 200000000)

(remove-hook 'text-mode-hook #'vi-tilde-fringe-mode)

(setq pandoc-binary "/opt/homebrew/bin/pandoc")

(setq pandoc-data-dir "/Users/ismailefetop/.config/doom/etc/pandoc/")

(setq org-directory "~/.orgs/org/")

(setq org-agenda-files '("~/uni/current-course/" "~/.orgs/org/" "~/ideas/" "/Users/ismailefetop/Library/Mobile Documents/com~apple~CloudDocs/org/"))

(setq org-confirm-babel-evaluate nil)

(setq my-keyboard-shortcut "SPC o a a n")

(defun my-send-keyboard-shortcut ()
  (interactive)
  (execute-kbd-macro (kbd my-keyboard-shortcut)))

;; (add-hook 'emacs-startup-hook 'my-send-keyboard-shortcut)

(use-package! org
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
        '(("t" "School Todo" entry (file+olp+datetree "~/uni/current-course/todo.org")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Class Note" entry (file+olp+datetree "~/uni/current-course/notes/%A.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
)

   (require 'org-auto-tangle)

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

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
  (olivetti-mode)
  (setq hl-line-mode nil)
  (menu-bar--display-line-numbers-mode-none))

(defun efe/undo-reading-mode ()
  "undo reading mode."
  (interactive)
  (hide-mode-line-mode -1)
  (setq olivetti-mode nil)
  (setq hl-line-mode t)
  (menu-bar--display-line-numbers-mode-absolute))

(defun efe/export-to-docx ()
  "Output to docx using pandoc-mode"
  (interactive)
  (pandoc-mode)
  (execute-kbd-macro (kbd "C-c / O W d b b r"))
  (setq pandoc-mode nil)
  )

(defun insert-idiom-template ()
  "Inserts a template for an idiom."
  (interactive)
  (insert "\n* Idiom\n"
          "- *Meaning:*\n"
          "- *Example Sentence:*\n"
          "- *Source:*\n"
          "- *Date:*\n"))

(set-file-template! "\\.org$" :trigger "__week.org" :mode 'org-mode)

(setq browse-url-mailto-function 'browse-url-generic)
(setq browse-url-generic-program "open")

(require 'openwith)
(openwith-mode t)
(setq openwith-associations
      '(("\\.pdf\\'" "open" (file))
        ("\\.docx\\'" "open" (file))
        ("\\.jpg\\'" "open" (file))
        ("\\.jpeg\\'" "open" (file))
        ("\\.png\\'" "open" (file))
        ;; ("\\.svg\\'" "open" (file))
        ("\\.gif\\'" "open" (file))
        ;; Add more image formats as needed
        ))

(use-package! mw-thesaurus
  :defer t
  :commands mw-thesaurus-lookup-dwim
  :hook (mw-thesaurus-mode . variable-pitch-mode)
  :config
  (map! :map mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)

  ;; window on the right side
  (add-to-list
   'display-buffer-alist
   `(,mw-thesaurus-buffer-name
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.3))))

(setq dictionary-server "dict.org")

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(after! gcmh
  (setq gcmh-high-cons-threshold (* 64 1048576)))
