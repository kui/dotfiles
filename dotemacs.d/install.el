;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-

;; インストールするパッケージリスト
(setq target-packages
      (list 'helm
            'auto-complete
            'tabbar
            'flymake-ruby
            'rsense
            'color-theme
            'color-theme-twilight
            'markdown-mode
            ))

(message "** start inital installation **")
(let ()
  (dolist (pkg target-packages)
    (if (not (package-installed-p pkg))
        (package-install pkg)
      (message "* %s\tinstalled" pkg))
    ))
(message "** done inital installation **")
