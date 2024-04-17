;;; lighthaus-theme.el --- lighthaus
;;; Version: 1.0
;;; Commentary:
;;; A theme called lighthaus
;;; Code:

(deftheme lighthaus "DOCSTRING for lighthaus")
  (custom-theme-set-faces 'lighthaus
   '(default ((t (:foreground "#fdfbdf" :background "#18181d" ))))
   '(cursor ((t (:background "#fdfbdf" ))))
   '(fringe ((t (:background "#18181d" ))))
   '(mode-line ((t (:foreground "#fdfbdf" :background "#18181d" ))))
   '(region ((t (:background "#504945" ))))
   '(secondary-selection ((t (:background "#3e3834" ))))
   '(font-lock-builtin-face ((t (:foreground "#fe8019" ))))
   '(font-lock-comment-face ((t (:foreground "#949494" ))))
   '(font-lock-function-name-face ((t (:foreground "#d05e26" ))))
   '(font-lock-keyword-face ((t (:foreground "#c192ae" ))))
   '(font-lock-string-face ((t (:foreground "#d05e26" ))))
   '(font-lock-type-face ((t (:foreground "#d3869b" ))))
   '(font-lock-constant-face ((t (:foreground "#d3869b" ))))
   '(font-lock-variable-name-face ((t (:foreground "#83a598" ))))
   '(minibuffer-prompt ((t (:foreground "#d05e26" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
   )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'lighthaus)

;;; lighthaus-theme.el ends here