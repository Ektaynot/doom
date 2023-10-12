(setq user-full-name "İsmail Efe Top"
      user-mail-address "ismailefetop@gmail.com")

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type t)

(setq org-directory "~/.orgs/org/")

(setq org-agenda-span 8)

(setq org-agenda-files '("~/uni/current-course/" "~/.orgs/org/" "/Users/ismailefetop/Library/Mobile Documents/com~apple~CloudDocs/org/"))
(setq doom-font (font-spec :family "JetBrains Mono" :size 22))

(setq confirm-kill-emacs nil)

(after! org
  (setq org-capture-templates
        '(("t" "School Todo" entry (file+headline "~/uni/current-course/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Class Note" entry (file+datetree "~/uni/current-course/notes/%A.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
        )

(setq browse-url-mailto-function 'browse-url-generic)
(setq browse-url-generic-program "open")

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

;; -------- to open emacs with orgmode --------
;; Define the keyboard shortcut as a string
(setq my-keyboard-shortcut "SPC o a a n")

;; Define a function to execute the keyboard shortcut
(defun my-send-keyboard-shortcut ()
  (interactive)
  (execute-kbd-macro (kbd my-keyboard-shortcut)))

;; Call the function when Emacs starts up
(add-hook 'emacs-startup-hook 'my-send-keyboard-shortcut)
;; ---------------------------------------------

(add-hook 'org-mode-hook (lambda () (setq org-agenda-span 8)))

(setq org-confirm-babel-evaluate nil)

(require 'openwith)
(openwith-mode t)
(setq openwith-associations
      '(("\\.pdf\\'" "open" (file))
        ("\\.docx\\'" "open" (file))))
