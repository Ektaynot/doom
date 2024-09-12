;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; eye-candy
(package! olivetti)

;; biblio
(package! citeproc)
(package! org-ref)

;; org
(package! pandoc-mode)
(package! org-auto-tangle)
(package! org-pandoc-import :recipe (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))

;; misc
(package! openwith)
(package! reveal-in-osx-finder)
(package! doom-snippets :ignore t)
(package! crux)
(package! ready-player)
(package! kana)
(package! valign)

;; unused
;;(package! org-ros)
;;(package! org-modern)
;;(package! org-chef)
;;(package! insert-shebang)
;;(package! autothemer)

;; Custom Themes
;;(package! kanagawa-theme)
;;(package! catppuccin-theme)
;;(package! eink-theme)
;;(package! rose-pine-emacs :recipe
;;  (:host github
;;   :repo "thongpv87/rose-pine-emacs"
;;   :branch "master"))

