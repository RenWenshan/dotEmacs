;; ---- Server ----
(server-start)
;; ---- Server Ends ---

;; ---- Nicer ----

;; Color theme
(color-theme-initialize)
(color-theme-matrix)
;; (color-theme-subtle-hacker)

;; Don't show start up sceen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight regions and add special behaviors to regions
(setq transient-mark-mode t)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; Line-wrapping
(set-default 'fill-column 80)

;; Prevent beep
(setq visible-bell t)

;; See matching parens
(show-paren-mode t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Support trash
(setq delete-by-moving-to-trash t)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; ---- Nicer end ----


;; ---- Programming ----
;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(require 'yasnippet-bundle)

;; == Python ==

(load-file "emacs-for-python/epy-init.el")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/python-mode")
;; (setq py-install-directory "~/.emacs.d/site-lisp/python-mode")
;; (require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; (require 'auto-complete)

;; Electric Pairs
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map "\"" 'electric-pair)
	    (define-key python-mode-map "\'" 'electric-pair)
	    (define-key python-mode-map "(" 'electric-pair)
	    (define-key python-mode-map "[" 'electric-pair)
	    (define-key python-mode-map "{" 'electric-pair)))
(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))
;; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda ()
			       (define-key python-mode-map "\C-m" 'newline-and-indent)))


;; Lambda
(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))


;; path to the python interpreter, e.g.: ~rw/python27/bin/python2.7
(setq py-python-command "ipython")
(autoload 'python-mode "python-mode" "Python editing mode." t)


;; pylookup
;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
(setq pylookup-dir "~/.emacs.d/site-lisp/pylookup")
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(global-set-key "\C-ch" 'pylookup-lookup)

;; (require 'python-pep8)
;; (require 'python-pylint)

;; == Python ==

;; ---- Programming end ----



;; ---- Code Reading ----
(require 'xcscope)
(require 'ecb)

(require 'highlight-symbol)

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)

;; ---- Code Reading end ----
