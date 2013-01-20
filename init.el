;; This is the main configuration file of GNU Emacs
;;
;; Author: Wenshan Ren (renws1990@gmail.com)
;; Blog: wenshanren.org
;;
;; Keybindings:
;; F5        call last kmacro
;; F6        close current buffer
;; F7        emms seek backward
;; F8        emms pause
;;
;;
;; Prerequisites:
;;  1. magit
;;     Download and compile
;;
;;  2. pycheckers
;;     Write your own script and make it executable
;;
;;  3. epc
;;     $ sudo pip install epc
;;     In Emacs, M-x el-get-install epc
;;
;;  4. argparse
;;     $ sudo pip install argparse
;;

;; set load-path
(add-to-list 'load-path "~/.emacs.d/dotEmacs")
(progn (cd "~/.emacs.d/dotEmacs")
       (normal-top-level-add-subdirs-to-load-path))

;; start server, used for emacsclient
(server-start)

;; customized variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring smiley stamp track))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )




;;----------------------------------------------------------
;; ---- BEGIN nicer Emacs ----
;;----------------------------------------------------------
;;
;; this part is for making Emacs easier to use

;; avoid creating backup files in the same folder
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backup")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backup" t)))

;; force English Emacs environment
(set-language-environment 'English)

;; bind call last keyboard marco to a convinent key
(global-set-key [f5] 'call-last-kbd-macro)

;; don't show start up sceen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight regions and add special behaviors to regions
(setq transient-mark-mode t)

;; display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; line-wrapping
(set-default 'fill-column 80)

;; prevent beep
(setq visible-bell t)

;; see matching parens
(show-paren-mode t)

;; don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; support trash
(setq delete-by-moving-to-trash t)

;; insert space as tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; displaying the lambda symbol
(require 'lambda-mode)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

;; ido-mode
(ido-mode 1)

;; turn off tool bar, scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

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

;; nicer zoom and window enlarge/decrese
(global-set-key (kbd "C->") 'text-scale-increase)
(global-set-key (kbd "C-<") 'text-scale-decrease)

(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)

;; easier close file, F6 is labeled Close" respectively on Microsoft Natural
;; 4000 keyboard
(global-set-key (kbd "<f6>") 'kill-this-buffer)

;; clear the buffer in eshell
(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; stop eshell cycle completion
(setq eshell-cmpl-cycle-completions nil)

;; max-specpdl-size
(setq max-specpdl-size 65525)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)))
;; kill unmodified buffers without prompt
(setq ibuffer-expert t)

;; display white spaces
(require 'blank-mode)

;; auto closing pairs
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; show line numbers by default
(global-linum-mode 1)

;; highlight a symbol
(require 'highlight-symbol)
(global-set-key [(control f8)] 'highlight-symbol-at-point)
(global-set-key [f8] 'highlight-symbol-next)
(global-set-key [(shift f8)] 'highlight-symbol-prev)
(global-set-key [(meta f8)] 'highlight-symbol-prev)

;; color theme tango-dark (Emacs24)
(add-to-list 'custom-theme-load-path "~/.emacs.d/dotEmacs/")
(load-theme 'tango-dark t)

;; indentation guide
(require 'highlight-indentation)

;; code folding with C-tab
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [C-tab] 'aj-toggle-fold)

;; enter for new line and indent
(local-set-key (kbd "RET") 'newline-and-indent)

;; speedbar that can be fired up in the same frame
(require 'sr-speedbar)

;; show all files and disable speedbar auto-refresh
(add-hook 'speedbar-mode-hook
          (lambda ()
            (speedbar-toggle-show-all-files)
            (speedbar-disable-update)))

;; insert current date, will be used by org-mode
(defun insert-org-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a")))

;; shortcut for (eval-print-last-sexp)
(global-set-key (kbd "C-j") 'eval-print-last-sexp)

;; name completion for describe function, variable, etc
(icomplete-mode 1)

;; capture image
(require 'capture-image)

;; remember last few window configurations, C-c left/right to switch
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; automatically refresh buffers
(global-auto-revert-mode t)

;; auto insert
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/dotEmacs/templates")
(setq auto-insert-query nil) ; turn of prompt
(define-auto-insert "\.py" "python-template.py")

;;----------------------------------------------------------
;; ---- END nicer ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN el-get ----
;;----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(el-get 'sync)
;;----------------------------------------------------------
;; ---- END el-get ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN flymake ----
;;----------------------------------------------------------

(load-library "flymake-cursor")

;; tex files checking, replaced texify with chktex
(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-q" "-v0" file-name)))

;; xml/html files checking, default setting not work
(defun flymake-xml-init ()
  (list "xmlstarlet"
        (list "val" "-e" "-q"
              (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))))

;; f7 to go to previous error, f8 to jump to next error
;; (global-set-key [f7] 'flymake-goto-prev-error)
;; (global-set-key [f8] 'flymake-goto-next-error)

;;----------------------------------------------------------
;; ---- END flymake ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Emacs Lisp ----
;;----------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; turn on flyspell
            (flyspell-mode 1)
            ;; turn on indentation guide by default
            (highlight-indentation-mode t)
            ))

;; turn on lambda mode by default, showing "lambda" as the lambda symbol
(add-hook 'emacs-lisp-mode-hook #'lambda-mode 1)

;;----------------------------------------------------------
;; ---- END Emacs Lisp ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Chinese input for Linux ----
;;----------------------------------------------------------

(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)
(ibus-define-common-key ?\C-/ nil)
(setq ibus-cursor-color '("red" "blue" "limegreen"))

;;----------------------------------------------------------
;; ---- END Chinese input for Linux ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN org-mode ----
;;----------------------------------------------------------

(setq load-path (cons "~/.emacs.d/dotEmacs/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/dotEmacs/org-mode/contrib/lisp" load-path))
(require 'org-install)

(add-hook 'org-mode-hook '(lambda ()
                            ;; use C-j to eval elisp expressions in org-mode
                            (define-key org-mode-map (kbd "C-j") 'eval-print-last-sexp)
                            ;; turn on flyspell-mode by default
                            (flyspell-mode 1)))

;;----------------------------------------------------------
;; ---- END org-mode ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN blogging ----
;;----------------------------------------------------------

;; use xml-rpc to post blogs on my English wordpress blog on OpenShift
(require 'xml-rpc)
(setq load-path (cons "~/.emacs.d/dotEmacs/org2blog" load-path))
(require 'org2blog-autoloads)
(setq org2blog/wp-blog-alist
      '(("wenshanren.org"
         :url "http://wenshanren.org/xmlrpc.php"
         :username "wenshan"
         :default-title "Hello World"
         :default-categories ("Linux")
         :tags-as-categories nil
         :wp-code nil
         :keep-new-lines t)
        )
      )

;;----------------------------------------------------------
;; ---- END blogging ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN magit ----
;;----------------------------------------------------------

;; this path should be changed along with magit installation
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(require 'magit)

;;----------------------------------------------------------
;; ---- END magit ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN yasnippet ----
;;----------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/dotEmacs/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;----------------------------------------------------------
;; ---- END yasnippet ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN auto-complete ----
;;----------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/dotEmacs/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dotEmacs/auto-complete/dict")
(ac-config-default)

;;----------------------------------------------------------
;; ---- END auto-complete ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN anything ----
;;----------------------------------------------------------

(add-to-list 'load-path "～/.emacs.d/dotEmacs/anything-config")
(require 'anything-config)

;; buffer switch
(global-set-key (kbd "C-x b")
                (lambda() (interactive)
                  (anything
                   :prompt "Switch to: "
                   :candidate-number-limit 10                 ;; up to 10 of each
                   :sources
                   '( anything-c-source-buffers               ;; buffers
                      anything-c-source-recentf               ;; recent files
                      anything-c-source-bookmarks             ;; bookmarks
                      anything-c-source-files-in-current-dir+ ;; current dir
                      anything-c-source-locate))))            ;; use 'locate'

;; search documents
(global-set-key (kbd "C-c I")  ;; i -> info
                (lambda () (interactive)
                  (anything
                   :prompt "Info about: "
                   :candidate-number-limit 3
                   :sources
                   '( anything-c-source-info-libc             ;; glibc docs
                      anything-c-source-man-pages             ;; man pages
                      anything-c-source-info-emacs))))        ;; emacs

;; emacs lisp hook
(add-hook 'emacs-lisp-mode-hook
          (lambda()
            ;; other stuff...
            ;; ...
            ;; put useful info under C-c i
            (local-set-key (kbd "C-c i")
                           (lambda() (interactive)
                             (anything
                              :prompt "Info about: "
                              :candidate-number-limit 5
                              :sources
                              '( anything-c-source-emacs-functions
                                 anything-c-source-emacs-variables
                                 anything-c-source-info-elisp
                                 anything-c-source-emacs-commands
                                 anything-c-source-emacs-source-defun
                                 anything-c-source-emacs-lisp-expectations
                                 anything-c-source-emacs-lisp-toplevels
                                 anything-c-source-emacs-functions-with-abbrevs
                                 anything-c-source-info-emacs))))
            )
          )

;;----------------------------------------------------------
;; ---- END anything ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN EMMS (Emacs MultiMedia System ) ----
;;----------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/dotEmacs/emms/")
(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-player-list '(emms-player-mplayer))
(global-set-key (kbd "<f7>") 'emms-seek-backward)
(global-set-key (kbd "<f8>") 'emms-pause)


;;----------------------------------------------------------
;; ---- END EMMS (Emacs MultiMedia System)----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN go lang ----
;;----------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/dotEmacs/go")
(require 'go-mode-load)

;;----------------------------------------------------------
;; ---- END go lang ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ----  BEGIN Python ----
;;----------------------------------------------------------

;; load python.el
(require 'python)

;; use ipython
(setq python-command "ipython")
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-string-code
 "';'.join(__IP.complete('''%s'''p))\n"
 python-shell-completion-module-string-code "")

;; load indentation guide by default
(add-hook 'python-mode-hook
          '(lambda ()
             (highlight-indentation-mode t)
             ))

;; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda ()
                               (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; display lambda for python
(add-hook 'python-mode-hook #'lambda-mode 1)

;; pylookup
;; Usage:
;; C-c h term
;;
;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
(setq pylookup-dir "~/.emacs.d/dotEmacs/pylookup")
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(global-set-key "\C-ch" 'pylookup-lookup)

;; flymake for python, need create a script named pycheckers
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

;; highlight breakpoint(s)
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)

;; add a breakpoint with C-c C-t
(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)

;; add a log print statement with C-c C-d
(defun python-add-log-print ()
  (interactive)
  (newline-and-indent)
  (insert "if DEBUG: print '----- wenshan log -----'")
  (highlight-lines-matching-regexp "^[ ]*if DEBUG: print '----- wenshan log -----"))

(define-key python-mode-map (kbd "C-c C-d") 'python-add-log-print)

;; jedi for auto-completion
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

;;----------------------------------------------------------
;; ---- End Python ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Perl ----
;;----------------------------------------------------------

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

(defun flymake-goto-next-error-disp ()
  "Go to next error in err ring, then display error/warning."
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-current-error))

(defun flymake-goto-prev-error-disp ()
  "Go to previous error in err ring, then display error/warning."
  (interactive)
  (flymake-goto-prev-error)
  (flymake-display-current-error))

;;----------------------------------------------------------
;; ---- END Perl ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Octave ----
;;----------------------------------------------------------

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;;----------------------------------------------------------
;; ---- END Octave ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN web development ----
;;----------------------------------------------------------

;; auto close tag after </
(setq nxml-slash-auto-complete-flag t)

;; rename a tag and its closing tag at the same time
(require 'rename-sgml-tag)
(defun nxml-mode-additional-keys ()
  "Key bindings to add to `nxml-mode'."
  (define-key nxml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
  )
(add-hook 'nxml-mode-hook 'nxml-mode-additional-keys)

;;----------------------------------------------------------
;; ---- END web development ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN microblog ----
;;----------------------------------------------------------

(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-allow-insecure-server-cert t)
(setq twittering-oauth-use-ssl nil)
(setq twittering-use-ssl nil)
(twittering-enable-unread-status-notifier)
(setq-default twittering-icon-mode t)
(setq twittering-initial-timeline-spec-string `(":home@sina"))

;;----------------------------------------------------------
;; ---- END microblog ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN IRC ----
;;----------------------------------------------------------

(require 'erc)

;; hide time-stamp
(setq erc-hide-timestamps t)

;; highlight nicknames
(and
 (load-library "erc-highlight-nicknames")
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))

;; save logs
(setq erc-log-channels-directory "~/.erc/logs/")
;; log files automatically written when part a channel or quit
(setq erc-save-buffer-on-part t)


;; login as Meatball_py
(defun myerc ()
  (interactive)
  (let
      ((password-cache nil))
    (erc
     :server "irc.freenode.net"
     :port "6667"
     :nick "Meatball_py"
     :password (password-read (format "password for Meatball at freenode?")))))

;; invoke fly-spell by default
(add-hook 'erc-mode-hook (lambda ()
                           (flyspell-mode 1)))

;;----------------------------------------------------------
;; ---- END IRC ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Chrome edit ----
;;----------------------------------------------------------

;; this is used for "Edit with Emacs" Chrome extension
(require 'edit-server)
(edit-server-start)

;;----------------------------------------------------------
;; ---- END Chrome edit ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Markdown support ----
;;----------------------------------------------------------

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;----------------------------------------------------------
;; ---- END Markdown support ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN default directory ---
;;----------------------------------------------------------

(cd "~/Dropbox" )

;;----------------------------------------------------------
;; ---- END default directory ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN MISC ---
;;----------------------------------------------------------

;; ;; script for retrieving el-get, comment this out once finished
;; (url-retrieve
;;  "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
;;  (lambda (s)
;;    (let (el-get-master-branch)
;;      (goto-char (point-max))
;;      (eval-print-last-sexp))))

;;----------------------------------------------------------
;; ---- BEGIN MISC ---
;;----------------------------------------------------------
