;; first year in uni, mba2022

;; my contact informations
(setq user-full-name "İsmail Efe Top"
      user-mail-address "ismailefetop@gmail.com")

;; doom emacs theme setter
(setq doom-theme 'doom-dracula)

;; changing doom emacs's font
(setq doom-font (font-spec :family "JetBrains Mono" :size 22))

;; to display line numbers
(setq display-line-numbers-type t)

;; do not confirm when you want to exit
(setq confirm-kill-emacs nil)

;; to start the agende from the current day 
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "+0d")

;; how many days should org mode display 
(setq org-agenda-span 8)
(add-hook 'org-mode-hook (lambda () (setq org-agenda-span 8)))

;; setting the main org directory
(setq org-directory "~/.orgs/org/")

;; different places that org files can live
(setq org-agenda-files '("~/uni/current-course/" "~/.orgs/org/" "/Users/ismailefetop/Library/Mobile Documents/com~apple~CloudDocs/org/"))

;; trusting the org-blocks in org automatically
(setq org-confirm-babel-evaluate nil)

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

;; org capture templates
(after! org
  (setq org-capture-templates
        '(("t" "School Todo" entry (file+headline "~/uni/current-course/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Class Note" entry (file+datetree "~/uni/current-course/notes/%A.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
        )

;; ---------------------------------------------
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
;; ---------------------------------------------

;; changes the default mail client to the default mac one
(setq browse-url-mailto-function 'browse-url-generic)
(setq browse-url-generic-program "open")

;; changes the default emacs openers to mac 
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
;; ---------------------------------------------
