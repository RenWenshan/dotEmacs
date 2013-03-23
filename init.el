;; This is the main configuration file of GNU Emacs
;;
;; Author: 任文山 (Ren Wenshan <renws1990@gmail.com>)
;; Blog: wenshanren.org



;;----------------------------------------------------------
;; ---- BEGIN basic configuration ----
;;----------------------------------------------------------

;; set load-path
(add-to-list 'load-path "~/.emacs.d/dotEmacs")
(progn (cd "~/.emacs.d/dotEmacs")
       (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/el-get")
(progn (cd "~/.emacs.d/el-get")
       (normal-top-level-add-subdirs-to-load-path))

;; start server, used for emacsclient
(server-start)

;; customized variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eclim-eclipse-dirs (quote ("~/eclipse")))
 '(erc-modules (quote
                (autojoin button completion fill irccontrols list log match
                          menu move-to-prompt netsplit networks noncommands
                          readonly ring smiley stamp track)))
 '(user-full-name "任文山 (Ren Wenshan)")
 '(user-email-address "renws1990@gmail.com")
 )

;;----------------------------------------------------------
;; ---- END basic configuration ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN nicer Emacs ----
;;----------------------------------------------------------
;;
;; this part is for making Emacs easier to use

;; join line with M-j
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; avoid creating backup files in the same folder
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backup")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backup" t)))

;; force English Emacs environment
(set-language-environment 'English)

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

;; line width (fixed to 79)
(setq-default fill-column 79)

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
(load-theme 'tango-dark t)

;; indentation guide
(require 'highlight-indentation)

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

;; shell mode
;; fix "wrong type argument: characterp, return"
(add-hook 'term-mode-hook
          #'(lambda ()
              (setq autopair-dont-activate t) ;; for emacsen < 24
              (autopair-mode -1))             ;; for emacsen >= 24
          )
;; use zsh instead of bash
(defun sh ()
  (interactive)
  (ansi-term "/bin/zsh"))

;; completion
(require 'shell-completion)

;; remove ^M
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; clear shell
(defun clear-shell ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

;; enable region narrowing
(put 'narrow-to-region 'disabled nil)

;; turn off cursor blink
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))

;; csv-mode, please install this by ELPA
(require 'csv-mode)

;; search the kill ring with M-C-y
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))

(global-set-key "\M-\C-y" 'kill-ring-search)

;; csv-mode
(require 'csv-mode)

;; folding/unfolding with C-c C-h
(global-set-key (kbd "C-c C-h") 'hs-toggle-hiding)

;; eldoc
(require 'eldoc)

;; font
(set-default-font "Dejavu Sans Mono 11")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 18)))

;; package
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;----------------------------------------------------------
;; ---- END nicer ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN el-get ----
;;----------------------------------------------------------
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

;; f7 to go to previous error, f8 to jump to next error
;; (global-set-key [f7] 'flymake-goto-prev-error)
;; (global-set-key [f8] 'flymake-goto-next-error)

;;----------------------------------------------------------
;; ---- END flymake ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Lisp ----
;;----------------------------------------------------------

;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (eldoc-mode t)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (paredit-mode +1)
            (eldoc-mode t)))

;;----------------------------------------------------------
;; ---- END Lisp ----
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
            ;; paredit mode
            (paredit-mode +1)
            ;; eldoc
            (eldoc-mode t)
            ;; folding
            (hs-minor-mode t)
            ))

;; turn on lambda mode by default, showing "lambda" as the lambda symbol
(add-hook 'emacs-lisp-mode-hook #'lambda-mode 1)

;;----------------------------------------------------------
;; ---- END Emacs Lisp ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Clojure ----
;;----------------------------------------------------------
(add-hook 'clojure-mode-hook 'paredit-mode)

;;----------------------------------------------------------
;; ---- END Clojure ----
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
(setq load-path (cons "~/.emacs.d/el-get/org-mode/contrib/lisp" load-path))
(setq load-path (cons "~/.emacs.d/el-get/org-mode/lisp" load-path))
(require 'org-install)

(add-hook 'org-mode-hook '(lambda ()
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

;; turn on flyspell mode when editing commit messages
(add-hook 'magit-log-edit-mode-hook
          '(lambda ()
             (flyspell-mode 1)))

;; turn off magit highlighting
(eval-after-load "magit"
  ;; no highlight
  '(defun magit-highlight-section ()))

;;----------------------------------------------------------
;; ---- END magit ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN yasnippet ----
;;----------------------------------------------------------

(require 'yasnippet)
(yas-global-mode 1)

;;----------------------------------------------------------
;; ---- END yasnippet ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN auto-complete ----
;;----------------------------------------------------------

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/dotEmacs/auto-complete/dict")
(ac-config-default)

;;----------------------------------------------------------
;; ---- END auto-complete ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN anything ----
;;----------------------------------------------------------

(require 'anything-config)

;; buffer switch
(global-set-key (kbd "C-x b")
                (lambda() (interactive)
                  (anything
                   :prompt "Switch to: "
                   :candidate-number-limit 10
                   :sources
                   '( anything-c-source-buffers
                      anything-c-source-recentf
                      anything-c-source-bookmarks
                      anything-c-source-files-in-current-dir+
                      anything-c-source-locate))))

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

(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-player-list '(emms-player-mplayer))
(global-set-key (kbd "<f5>") 'emms-pause)
(global-set-key (kbd "<f7>") 'emms-seek-backward)
(global-set-key (kbd "<f8>") 'emms-seek-forward)


;;----------------------------------------------------------
;; ---- END EMMS (Emacs MultiMedia System)----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN go lang ----
;;----------------------------------------------------------

(require 'go-mode-load)

;;----------------------------------------------------------
;; ---- END go lang ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ----  BEGIN Python ----
;;----------------------------------------------------------

;; load python.el
(require 'python)

;; elpy: Emacs Python Development Environment
(package-initialize)
(elpy-enable)

;; grammar checking
(setq python-check-command "pycheckers")

;; use ipython
(elpy-use-ipython)

;; load indentation guide by default
(add-hook 'python-mode-hook
          '(lambda ()
             (highlight-indentation-mode t)
             (define-key python-mode-map "\C-m" 'newline-and-indent)
             (hs-minor-mode t)))

;; display lambda for python
(add-hook 'python-mode-hook #'lambda-mode 1)

;; add a breakpoint with C-c C-b
(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)

;; add a log print statement
(defun python-add-log-print ()
  (interactive)
  (newline-and-indent)
  (insert "if DEBUG: print '----- wenshan log -----'")
  (highlight-lines-matching-regexp
   "^[ ]*if DEBUG: print '----- wenshan log -----"))

;; enter interactive python
(defun python-interactive ()
  (interactive)
  (insert "!import code; code.interact(local=vars())"))

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


(define-prefix-command 'cm-map nil "Outline-")

(define-key cm-map "q" 'hide-sublevels)
(define-key cm-map "t" 'hide-body)
(define-key cm-map "o" 'hide-other)
(define-key cm-map "c" 'hide-entry)
(define-key cm-map "l" 'hide-leaves)
(define-key cm-map "d" 'hide-subtree)

(define-key cm-map "a" 'show-all)
(define-key cm-map "e" 'show-entry)
(define-key cm-map "i" 'show-children)
(define-key cm-map "k" 'show-branches)
(define-key cm-map "s" 'show-subtree)

(define-key cm-map "u" 'outline-up-heading)
(define-key cm-map "n" 'outline-next-visible-heading)
(define-key cm-map "p" 'outline-previous-visible-heading)
(define-key cm-map "f" 'outline-forward-same-level)
(define-key cm-map "b" 'outline-backward-same-level)
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

;; xml/html files checking, default setting not work
(defun flymake-xml-init ()
  (list "xmlstarlet"
        (list "val" "e"
              (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))))

;;; Mozilla REPL,
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(require 'moz)
(require 'json)

(defun moz-update (&rest ignored)
  "Update the remote mozrepl instance"
  (interactive)
  (comint-send-string (inferior-moz-process)
    (concat "content.document.body.innerHTML="
             (json-encode (buffer-string)) ";")))

(defun moz-enable-auto-update ()
  "Automatically the remote mozrepl when this buffer changes"
  (interactive)
  (add-hook 'after-change-functions 'moz-update t t))

(defun moz-disable-auto-update ()
  "Disable automatic mozrepl updates"
  (interactive)
  (remove-hook 'after-change-functions 'moz-update t))

;;----------------------------------------------------------
;; ---- END web development ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Java ----
;;----------------------------------------------------------

(require 'eclim)
(global-eclim-mode)

;; eclipse dir
(custom-set-variables
 '(eclim-eclipse-dirs '("~/eclipse")))

;; display compilation error messages in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

;;----------------------------------------------------------
;; ---- END Java ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN C ----
;;----------------------------------------------------------

(setq-default c-default-style "linux"
              c-basic-offset 4)

;;----------------------------------------------------------
;; ---- END C ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN microblog ----
;;----------------------------------------------------------

(require 'weibo)
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
     :password (password-read (format "password for Meatball at freenode? ")))))

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

(autoload 'markdown-mode
  "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;----------------------------------------------------------
;; ---- END Markdown support ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Email client ----
;;----------------------------------------------------------
(require 'mu4e)

;; default
(setq mu4e-maildir "~/Maildir")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "renws1990@gmail.com"
 user-full-name  "Ren Wenshan"
 message-signature
 (concat
  "任文山 (Ren Wenshan)\n"
  "Email: renws1990@gmail.com\n"
  "Blog: wenshanren.org\n"
  "Douban: www.douban.com/people/renws\n"
  "\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; add attachment using dired
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; ;; enable inline images
;; (setq mu4e-view-show-images t)
;; ;; use imagemagick, if available
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))

;;----------------------------------------------------------
;; ---- END Email client ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- Begin w3m web browser ----
;;----------------------------------------------------------
(require 'w3m-load)
;;----------------------------------------------------------
;; ---- END w3m web browser----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Instant Message ---
;;----------------------------------------------------------
(load "jabber-autoloads")
(setq jabber-account-list
      '(("renws1990@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl)
         (:port . 443))))
;;----------------------------------------------------------
;; ---- END Instant Message ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN epa ---
;;----------------------------------------------------------

(require 'epa-file)
(epa-file-enable)

;; always use symmetric encryption
(setq epa-file-encrypt-to nil)
;; turn on passphrase cache, so don't need to type in password for every save
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; turn on auto save
(setq epa-file-inhibit-auto-save nil)

;;----------------------------------------------------------
;; ---- END epa ---
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
