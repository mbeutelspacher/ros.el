((emacs-lisp-mode
  (eval . (flycheck-mode))
  (eval . (flycheck-cask-setup))
  (eval . (checkdoc-minor-mode))
  (eval . (smartparens-mode))
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (sentence-end-double-space . t)
  (emacs-lisp-docstring-fill-column . 75)))
