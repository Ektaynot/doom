(setq user-full-name "İsmail Efe Top"
      user-mail-address "ismailefetop@gmail.com")

(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "JetBrains Mono" :size 22))

(setq display-line-numbers-type t)

(setq confirm-kill-emacs nil)

(setq org-directory "~/.orgs/org/")

(setq org-agenda-files '("~/uni/current-course/" "~/.orgs/org/" "/Users/ismailefetop/Library/Mobile Documents/com~apple~CloudDocs/org/"))

(setq org-confirm-babel-evaluate nil)

(setq my-keyboard-shortcut "SPC o a a n")

(defun my-send-keyboard-shortcut ()
  (interactive)
  (execute-kbd-macro (kbd my-keyboard-shortcut)))

(add-hook 'emacs-startup-hook 'my-send-keyboard-shortcut)

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
        '(("t" "School Todo" entry (file+headline "~/uni/current-course/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Class Note" entry (file+datetree "~/uni/current-course/notes/%A.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
        )

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
        ("\\.gif\\'" "open" (file))
        ;; Add more image formats as needed
        ))

(setq olivetti-body-width 100)
