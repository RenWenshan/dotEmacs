(add-to-list 'load-path "~/.emacs.d/dotEmacs")
;; start server, used for emacsclient
(server-start)

;; ;; ---- Chinese input ----
;; (require 'ibus)
;; (add-hook 'after-init-hook 'ibus-mode-on)
;; (ibus-define-common-key ?\C-/ nil)
;; (setq ibus-cursor-color '("red" "blue" "limegreen"))
;; ;; ---- Chinese input end ---

;; ---- Spelling Checker for Mac ---
(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-program-name "aspell"
      ispell-dictionary "english"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))
;; ---- END Spelling Checker for Mac ---

;; ---- GPG for Mac ---
(add-to-list 'exec-path "/usr/local/bin")
;; ---- END GPG for Mac---

;; ---- Org mode ---
(setq load-path (cons "~/.emacs.d/dotEmacs/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/dotEmacs/org-mode/contrib/lisp" load-path))
(require 'org-install)
;; ---- End Org mode ---

;; ---- Blog posting ---
;; use xml-rpc to post blogs on my English wordpress blog on OpenShift
(require 'xml-rpc)
(setq load-path (cons "~/.emacs.d/dotEmacs/org2blog" load-path))
(require 'org2blog-autoloads)
(setq org2blog/wp-blog-alist
      '(("wordpress"
         :url "http://blog-wenshan.rhcloud.com/xmlrpc.php"
         :username "admin"
         :default-title "Hello World"
         :default-categories ("Linux")
         :tags-as-categories nil
         :wp-code nil
         :keep-new-lines t)
        ))
;; ---- End Blog posting ---

;; ---- Nicer ----
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backup")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backup" t)))

(set-language-environment 'English)

;; C-w to backward kill a word
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; bind call last keyboard marco to a convinent key
(global-set-key [f5] 'call-last-kbd-macro)

;; ;; Color theme
;; (color-theme-initialize)
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

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; ido-mode
(ido-mode 1)

;; turn off tool bar, scroll bar and menu bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; copy-paste
(setq x-select-enable-clipboard t)

;; line width (fixed to 80)
(setq-default fill-column 80)

;; Ctrl-x Ctrl-m to invoke M-x sequence
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; nicer buffers switching
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; display white spaces
(require 'blank-mode)

;; avoid eshell cycle completions
(setq eshell-cmpl-cycle-completions nil)

;; ---- Nicer end ----


;; ---- Programming ----
(progn (cd "~/.emacs.d/dotEmacs")
	(normal-top-level-add-subdirs-to-load-path))

;; ;; == BEGIN git ==
;; this path should be changed along with magit installation
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(require 'magit)
;; ;; == END git ==

;; == BEGIN yasnippet ==
(add-to-list 'load-path "~/.emacs.d/dotEmacs/yasnippet-0.5.9")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/dotEmacs/yasnippet-0.5.9/snippets")
;; == END yasnippet ==


;; == BEGIN auto-complete ==
(add-to-list 'load-path "~/.emacs.d/dotEmacs/auto-complete-1.3.1")
(require 'auto-complete-config)
(add-to-list 'load-path "~/.emacs.d/dotEmacs/auto-complete-1.3.1")
(ac-config-default)
;; == END auto-complete ==


;; == BEGIN autopair ==
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)
;; == END autopair ==

;; == BEGIN go ==
(add-to-list 'load-path "~/.emacs.d/dotEmacs/go")
(require 'go-mode-load)
;; == END go ==

;; ;; == BEGIN Python ==
;; ;; pymacs
;; (add-to-list 'load-path "~/.emacs.d/dotEmacs/python/Pymacs")
;; (require 'pymacs)

;; ;; rope
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; ;; bind RET to py-newline-and-indent
;; (add-hook 'python-mode-hook '(lambda ()
;;                                (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; ;; Lambda
;; (require 'lambda-mode)
;; (add-hook 'python-mode-hook #'lambda-mode 1)
;; (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

;; ;; pylookup
;; ;; Usage:
;; ;; C-c h term
;; ;;
;; ;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
;; (setq pylookup-dir "~/.emacs.d/dotEmacs/pylookup")
;; (add-to-list 'load-path pylookup-dir)

;; ;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)

;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)
;; (global-set-key "\C-ch" 'pylookup-lookup)


;; ;; pyflakes, M-x python-check
;; (setq python-check-command "pyflakes")

;; ;; flymake for python
;; (add-to-list 'load-path "~/.emacs.d/dotEmacs")

;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                'flymake-create-temp-inplace))
;;        (local-file (file-relative-name
;;             temp-file
;;             (file-name-directory buffer-file-name))))
;;       (list "pycheckers"  (list local-file))))
;;    (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.py\\'" flymake-pyflakes-init)))
;; (load-library "flymake-cursor")
;; (global-set-key [f10] 'flymake-goto-prev-error)
;; (global-set-key [f11] 'flymake-goto-next-error)

;; ;; == END Python ==


;; == BEGIN Perl ==
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")
; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)

(setq cperl-mode-hook 'my-cperl-customizations)

(defun my-cperl-customizations ()
  "cperl-mode customizations that must be done after cperl-mode loads"
  (outline-minor-mode)
  (abbrev-mode)

  (defun cperl-outline-level ()
    (looking-at outline-regexp)
    (let ((match (match-string 1)))
      (cond
       ((eq match "=head1" ) 1)
       ((eq match "package") 2)
       ((eq match "=head2" ) 3)
       ((eq match "=item"  ) 4)
       ((eq match "sub"    ) 5)
       (t 7)
       )))

  (setq cperl-outline-regexp  my-cperl-outline-regexp)
  (setq outline-regexp        cperl-outline-regexp)
  (setq outline-level        'cperl-outline-level)
)


(eval-after-load 'pde-load
  '(add-hook 'cperl-mode-hook (lambda ()
                                (outline-minor-mode 1))))

(defun perl-eval () "Run selected region as Perl code" (interactive)
   (shell-command-on-region (mark) (point) "perl "))
(global-set-key (kbd "<f9>") 'perl-eval)


(defun pde-perl-mode-hook ()
   ;; chmod when saving
  (when (and buffer-file-name
        (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
      (add-hook 'after-save-hook 'executable-chmod nil t))
  (set (make-local-variable 'compile-dwim-check-tools) nil))

(defun flymake-display-current-error ()
          "Display errors/warnings under cursor."
      (interactive)
      (let ((ovs (overlays-in (point) (1+ (point)))))
        (catch 'found
          (dolist (ov ovs)
            (when (flymake-overlay-p ov)
              (message (overlay-get ov 'help-echo))
              (throw 'found t))))))
(global-set-key (kbd "<f5>") 'flymake-display-current-error)

    (defun flymake-goto-next-error-disp ()
      "Go to next error in err ring, then display error/warning."
      (interactive)
      (flymake-goto-next-error)
      (flymake-display-current-error))

(global-set-key (kbd "<f8>") 'flymake-display-current-error)
    (defun flymake-goto-prev-error-disp ()
      "Go to previous error in err ring, then display error/warning."
      (interactive)
      (flymake-goto-prev-error)
      (flymake-display-current-error))

;; == END Perl ==

;; == BEGIN Octave ==
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; == END Octave ==

;; == BEGIN Web Dev ==
(setq load-path (cons "~/.emacs.d/dotEmacs/multi-web-mode" load-path))
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
;; == END Web Dev ==

;; ---- Programming end ----



;; ---- Code Reading ----
;; (require 'xcscope)
;; (require 'ecb)
;; (require 'ede)


(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)

;; ---- Code Reading end ----


;; ---- Microblog ---
(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-allow-insecure-server-cert t)
(setq twittering-oauth-use-ssl nil)
(setq twittering-use-ssl nil)
(twittering-enable-unread-status-notifier)
(setq-default twittering-icon-mode t)
(setq twittering-initial-timeline-spec-string `(":home@sina"))
;; ---- END Microblog

;; ---- BEGIN IRC ----
(require 'erc)
;; ---- End IRC ----

;; ---- BEGIN Email ----
;; external editor of thunderbird
(require 'tbemail)
;; ---- END Email

;; ---- BEGIN Chrome ---
(require 'edit-server)
(edit-server-start)
;; ---- END Chrome ---

;; ---- BEGIN Default directory ---
(setq default-directory "~/Dropbox" )
;; ---- END Default directory ---
