;; This is the main configuration file of GNU Emacs
;;
;; Author: 任文山 (Ren Wenshan <renws1990@gmail.com>)
;; Blog: http://wenshanren.org


;;; Code:
;;----------------------------------------------------------
;; ---- BEGIN basic configuration ----
;;----------------------------------------------------------

;; language and coding environment
(set-language-environment 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; set load-path
(add-to-list 'load-path "~/.emacs.d/el-get")
(progn (cd "~/.emacs.d/el-get")
       (normal-top-level-add-subdirs-to-load-path))

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
 '(erc-modules (quote
                (autojoin button completion fill irccontrols list log match
                          menu move-to-prompt netsplit networks noncommands
                          readonly ring smiley stamp track)))
 '(user-full-name "任文山 (Ren Wenshan)")
 '(user-email-address "renws1990@gmail.com")
 '(speedbar-default-position (quote left))
 )
(setq browse-url-browser-function 'browse-url-firefox)
;;----------------------------------------------------------
;; ---- END basic configuration ----
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

;; see matching parenthesis
(show-paren-mode t)

;; light colored parenthesis
(require 'parenface)
(set-face-foreground 'paren-face "DimGray")

;; don't truncate lines
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; trailing whitespace is unnecessary
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

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

;; highlight a symbol
(require 'highlight-symbol)
(global-set-key [(control f8)] 'highlight-symbol-at-point)
(global-set-key [f8] 'highlight-symbol-next)
(global-set-key [(shift f8)] 'highlight-symbol-prev)
(global-set-key [(meta f8)] 'highlight-symbol-prev)

;; indentation guide
(require 'highlight-indentation)

;; enter for new line and indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; speedbar that can be fired up in the same frame
(require 'sr-speedbar)

(defadvice sr-speedbar-open
  (after advice-sr-speedbar-disable-auto-update activate compile)
  "Turn off auto refresh"
  (interactive)
  (sr-speedbar-refresh-turn-off)
  )

(add-hook 'speedbar-mode-hook
          (lambda ()
            ;; initial path
            (cd "~/Dropbox")
            ;; show all files
            (speedbar-toggle-show-all-files)
            ;; disable speedbar auto-refresh
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

;; enable region narrowing
(put 'narrow-to-region 'disabled nil)

;; turn off cursor blink
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))

;; search the kill ring with M-C-y
(global-set-key "\M-\C-y" 'kill-ring-search)

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

(defun wenshan-edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

(defun delete-this-file ()
  "Delete (move to trash) the file that is associated with the
current buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (delete-file filename t)
    (kill-buffer)))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; C-c g for Google Search
(global-set-key (kbd "C-c g") 'google)

(require 'fill-column-indicator)

(add-hook 'prog-mode-hook
          ;; for all programming modes
          (lambda ()
            ;; turn on spell checker
            (flyspell-prog-mode)
            ;; turn on line numbering
            (linum-mode t)
            ;; indentation guide
            (highlight-indentation-mode t)
            ;; highlight symbol as point
            (highlight-symbol-mode t))
          )

(defun wenshan-split-window-vertical (&optional wenshan-number)
  "Split the current window into `wenshan-number' windows and
balance all windows"
  (interactive "P")
  (setq wenshan-number (if wenshan-number
                           (prefix-numeric-value wenshan-number)
                         2))
  (while (> wenshan-number 1)
    (split-window-right)
    (setq wenshan-number (- wenshan-number 1)))
  (balance-windows))

;; remember buffers that are opened
(desktop-save-mode 1)

;; rgrep
(global-set-key (kbd "C-c s") 'rgrep)

(defun open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.  If
the path is starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.  This command is
similar to `find-file-at-point' but without prompting for
confirmation.
"
  (interactive)
  (let ((path (thing-at-point 'filename)))
    (if (string-match-p "\\`https*://" path)
        (progn (browse-url path))
      (progn ; not starting “http://”
        (if (file-exists-p path)
            (progn (find-file path))
          (if (file-exists-p (concat path ".el"))
              (progn (find-file (concat path ".el")))
            (progn
              (when (y-or-n-p
                     (format "file doesn't exist: 「%s」. Create?" path))
                (progn (find-file path ))))))))))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; ace-jump-mode
(require 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)

;; dired configuration

;; always recursively delete directory
(setq dired-recursive-deletes 'always)
;; always recursively copy directory
(setq dired-recursive-copies 'always)

;; auto guess target
(setq dired-dwim-target t)

;; dired+, dired-details and dired-details+
(require 'dired+)
(require 'dired-details)
(require 'dired-details+)

;; always show symbolic link targets
(setq dired-details-hide-link-targets nil)

;; omit unimportant files
(setq-default dired-omit-mode nil
              dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
;; toggle omit mode C-o
(define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)

(defun ublt/dired-open-native ()
  "Open marked files (or the file the cursor is on) from dired."
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
         (n (length files)))
    (when (or (<= n 3)
              (y-or-n-p (format "Open %d files?" n)))
      (dolist (file files)
        (call-process "gnome-open"
                      nil 0 nil file)))))
(define-key dired-mode-map (kbd "s-o") 'ublt/dired-open-native)

(defun dired-do-shell-unmount-device ()
  (interactive)
  (save-window-excursion
    (dired-do-async-shell-command
     "umount" current-prefix-arg
     (dired-get-marked-files t current-prefix-arg))))
(define-key dired-mode-map (kbd "s-u") 'dired-do-shell-unmount-device)

(global-set-key (kbd "C-x C-j") 'dired-jump)

;; C-c C-b to backup current file, C-x C-j ~xy to delete them
(defun make-backup-current-file ()
  "Make a backup copy of current file.

The backup file name has the form 「‹name›~‹timestamp›~」, in the
same dir. If such a file already exist, it's overwritten.  If the
current buffer is not associated with a file, do nothing."
  (interactive)
  (let ((currentFileName (buffer-file-name)) backupFileName)
    (if (file-exists-p currentFileName)
        (progn
          (setq backupFileName
                (concat
                 currentFileName
                 "~"
                 (format-time-string "%Y%m%d_%H%M%S") "~"))
          (copy-file currentFileName backupFileName t)
          (message
           (concat "Backup saved as: "
                   (file-name-nondirectory backupFileName)))
          )
      (progn ; file doesn't exist happens when it's new file not yet saved.
        (message (format "file 「%s」 doesn't exist." currentFileName))))))
(global-set-key (kbd "C-c C-b") 'make-backup-current-file)

;; shell alias
(defcustom xen-shell-abbrev-alist nil "a list of shell abbrevs for Xen")
(setq xen-shell-abbrev-alist
      '(
        ("xen-help" . "xe help")
        ("xen-list-vm" . "xe vm-list")
        ("xen-list-tmpl" . "xe template-list")
        ("xen-vm-to-tmpl" . (concat "xe snapshot-export-to template "
                                    "snapshot-uuid=<snapshot-uuid> "
                                    "filename=<template-filename>"))
        )
      )

(defun xen-shell-commands (cmdAbbrev)
  "insert shell command from a selection prompt."
  (interactive
   (list
    (ido-completing-read "shell abbrevs:"
                         (mapcar (lambda (x) (car x)) xen-shell-abbrev-alist)
                         "PREDICATE" "REQUIRE-MATCH") ) )
  (progn
    (insert (cdr (assoc cmdAbbrev xen-shell-abbrev-alist)))
    ))

(setq scroll-step            1
      scroll-conservatively  10000)

;;; EPA
(require 'epa-file)
(epa-file-enable)

;; always use symmetric encryption
(setq epa-file-encrypt-to nil)
;; turn on passphrase cache, so don't need to type in password for every save
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; turn on auto save
(setq epa-file-inhibit-auto-save nil)

(defadvice find-tag (after advice-recenter-after-find-tag activate compile)
  "Recenter after executing find-tag"
  (recenter))

 (setq chinese-calendar-celestial-stem
          ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
          chinese-calendar-terrestrial-branch
          ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])


(global-set-key (kbd "C-S-<up>")     'buf-move-up)
(global-set-key (kbd "C-S-<down>")   'buf-move-down)
(global-set-key (kbd "C-S-<left>")   'buf-move-left)
(global-set-key (kbd "C-S-<right>")  'buf-move-right)
;;----------------------------------------------------------
;; ---- END nicer ----
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
(setq load-path (cons "~/.emacs.d/dotEmacs/org-mode/contrib/lisp" load-path))
(setq load-path (cons "~/.emacs.d/dotEmacs/org-mode/lisp" load-path))
(require 'org-install)

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s -n -r\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook '(lambda ()
                            ;; turn on flyspell-mode by default
                            (flyspell-mode 1)
                            ;; keybinding for editing source code blocks
                            (local-set-key (kbd "C-c s e")
                                           'org-edit-src-code)
                            ;; keybinding for inserting code blocks
                            (local-set-key (kbd "C-c s i")
                                           'org-insert-src-block)
                            ;; yas/expand
                            (local-set-key (kbd "C-<tab>")
                                           'yas/expand)
                            ;; insert inactive time-stamp
                            (local-set-key (kbd "C-x t")
                                           'org-time-stamp-inactive)
                            ))
(setq org-log-done 'time)

;; agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/Dropbox/log.org"
                         "~/Dropbox/wenshan-willowit/worklog.org"
                         "~/Dropbox/Fitness/Fitness.org"))
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

(setq org-agenda-custom-commands
      '(("h" "Habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          ))
        ))

(defun wenshan-org-calculate-clock ()
  "Calculate the time spent on a task. This trivial function is
used for adjusting purpose."
  (interactive)
  (let ((now (current-time)))
    (setq org-clock-out-time now)
    (save-excursion
      (widen)
      (beginning-of-line 1)
      (looking-at (concat "[ \t]*" org-keyword-time-regexp))
      (setq ts (match-string 2))
      (goto-char (+ 2 (match-end 0)))
      (looking-at "[[<]\\([^]>]+\\)[]>]")
      (setq te (match-string 0))
      (goto-char (match-end 0))
      (delete-region (point) (point-at-eol))
      (setq s (-
               (org-float-time (apply 'encode-time (org-parse-time-string te)))
               (org-float-time (apply 'encode-time (org-parse-time-string ts)))
               )
            h (floor (/ s 3600))
            s (- s (* 3600 h))
            m (floor (/ s 60))
            s (- s (* 60 s)))
      (insert " => " (format "%2d:%02d" h m)))))
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

(defun wenshan-erc ()
  "Log into freenode with less keystrokes"
  (interactive)
  (let
      ((password-cache nil))
    (erc
     :server "irc.freenode.net"
     :port "6667"
     :nick "Meatball_py"                ;set your username here
     :password (password-read (format "Your password for freenode? ")))))

;; invoke fly-spell by default
(add-hook 'erc-mode-hook (lambda ()
                           (flyspell-mode 1)))

;;----------------------------------------------------------
;; ---- END IRC ----
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
;; ---- BEGIN w3m web browser ----
;;----------------------------------------------------------
(require 'w3m-load)
;;----------------------------------------------------------
;; ---- END w3m web browser----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Douban ---
;;----------------------------------------------------------

(require 'douban-music-mode)

;;----------------------------------------------------------
;; ---- END Douban ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN shell ----
;;----------------------------------------------------------

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

;; execute region for shell script
(defun execute-shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))

;;----------------------------------------------------------
;; ---- END shell ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Syntax Checking ----
;;----------------------------------------------------------
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;;----------------------------------------------------------
;; ---- END Syntax Checking ----
;;----------------------------------------------------------




;;----------------------------------------------------------
;; ---- BEGIN git ----
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

;;; Eshell command completion
(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
              "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
              nil t)
        (push (match-string 1) commands)
        (when (match-string 2)
          (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
                             (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

;;----------------------------------------------------------
;; ---- END git ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN yasnippet ----
;;----------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.8.0")
(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-<tab>") 'yas/expand)

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
                      anything-c-source-info-emacs))))

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
                                 anything-c-source-info-emacs))))))

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
            ;; paredit mode
            (paredit-mode +1)
            ;; eldoc
            (eldoc-mode t)
            ;; folding
            (hs-minor-mode t)
            ))

;; turn on lambda mode by default, showing "lambda" as the lambda symbol
(add-hook 'emacs-lisp-mode-hook #'lambda-mode 1)

;; turn on paredit for minibuffer when "eval-expression"
(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;; convenient keybindings
(global-set-key (kbd "C-c e b") 'do-eval-buffer)
(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-c e f") 'eval-defun)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e s") 'scratch)
(global-set-key (kbd "C-c e p") 'eval-print-last-sexp)

(global-set-key (kbd "C-c h e") 'view-echo-area-messages)
(global-set-key (kbd "C-c h f") 'find-function)
(global-set-key (kbd "C-c h k") 'find-function-on-key)
(global-set-key (kbd "C-c h l") 'find-library)
(global-set-key (kbd "C-c h v") 'find-variable)
(global-set-key (kbd "C-c h V") 'apropos-value)

;;----------------------------------------------------------
;; ---- END Emacs Lisp ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN CEDET ----
;;----------------------------------------------------------
(semantic-mode 1)
(custom-set-variables
 '(semantic-default-submodes (quote (global-semantic-idle-completions-mode
                                     global-semantic-idle-scheduler-mode
                                     global-semanticdb-minor-mode
                                     global-semantic-idle-summary-mode
                                     global-semantic-mru-bookmark-mode)))
 '(semantic-idle-scheduler-idle-time 10)
 '(semanticdb-javap-classpath
   (quote ("/usr/lib/jvm/java-6-oracle/jre/lib/rt.jar")))
 '(cedet-android-sdk-root "~/android")
 )
(require 'semantic/ia)
(add-hook 'semantic-init-hook
          '(lambda ()
             (imenu-add-to-menubar "TAGS")))
;; if you want to enable support for gnu global
(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
(when (cedet-ectag-version-check)
  (semantic-load-enable-primary-exuberent-ctags-support))

(global-ede-mode t)

;; enable db-javap
(require 'semantic/db-javap)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key "\M-n" 'semantic-ia-complete-symbol-menu)
             (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
             (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
             (local-set-key "\C-cj" 'semantic-ia-fast-jump)
             (local-set-key "\C-cR" 'semantic-symref)
             (local-set-key "\C-cr" 'semantic-symref-symbol)
             (local-set-key "." 'semantic-complete-self-insert)
             (local-set-key ">" 'semantic-complete-self-insert)
             (add-to-list 'ac-sources 'ac-source-gtags)
             (add-to-list 'ac-sources 'ac-source-semantic)
             ))
;;----------------------------------------------------------
;; ---- END CEDET ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Clojure ----
;;----------------------------------------------------------
(add-hook 'clojure-mode-hook 'paredit-mode)

;;----------------------------------------------------------
;; ---- END Clojure ----
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

(setenv "PYTHONPATH"
        (concat "/home/openerp/.emacs.d/el-get/rope"
                ":/home/openerp/openerp/server"
                ":/home/openerp/openerp/addons"
                ":/home/openerp/openerp/web"))
;; grammar checking
(setq python-check-command "pycheckers")

(add-hook 'python-mode-hook
          '(lambda ()
             (jedi:setup)
             (hs-minor-mode t)
             (lambda-mode 1)))

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:server-args
      '("--sys-path" "~/openerp/addons"
        "--sys-path" "~/openerp/server"
        "--sys-path" "~/openerp/web"))

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "if DEBUG: import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)

;; add a log print statement
(defun python-add-log-print ()
  (interactive)
  (newline-and-indent)
  (insert "if DEBUG: print '----- wenshan log -----'")
  (highlight-lines-matching-regexp
   "^[ ]*if DEBUG: print '----- wenshan log -----"))

(defun python-interactive ()
  "Enter the interactive Python environment"
  (interactive)
  (progn
    (insert "!import code; code.interact(local=vars())")
    (move-end-of-line 1)
    (comint-send-input)))

(global-set-key (kbd "C-c i") 'python-interactive)

(defun python-run-this-buffer-file ()
  "Save current buffer and run it in a shell"
  (interactive)
  (progn
    (let ((old-buf (buffer-name (current-buffer)))
          (pyfile (expand-file-name (buffer-file-name (current-buffer))))
          (pyshell "*Python-Shell*"))
      (save-buffer)
      (shell pyshell)
      (insert (concat "python " pyfile))
      (comint-send-input)
      (switch-to-buffer old-buf)
      (display-buffer pyshell)
      )))

(define-key python-mode-map (kbd "C-c r") 'python-run-this-buffer-file)

;; Python send code
(add-to-list 'load-path "~/.emacs.d/dotEmacs/isend-mode.el")
(require 'isend)
(setq isend-skip-empty-lines nil)
(setq isend-strip-empty-lines nil)
(setq isend-delete-indentation t)
(setq isend-end-with-empty-line t)

(defadvice isend-send (after advice-run-code-sent activate compile)
  "Execute whatever sent to the (Python) buffer"
  (interactive)
  (let ((old-buf (buffer-name)))
    (progn
      (switch-to-buffer isend--command-buffer)
      (goto-char (point-max))
      (comint-send-input)
      (switch-to-buffer old-buf))))

;; Info documentation lookup
(require 'info-look)

(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))

;; ;; tree style viewer
;; (require 'jedi-direx)
;; (eval-after-load "python"
;;   '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))

;; Kivy
(require 'kivy-mode)
(add-to-list 'auto-mode-alist '("\\.kv$" . kivy-mode))
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

(defun pde-perl-mode-hook ()
  ;; chmod when saving
  (when (and buffer-file-name
             (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
    (add-hook 'after-save-hook 'executable-chmod nil t))
  (set (make-local-variable 'compile-dwim-check-tools) nil))

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

(setq auto-mode-alist (cons '("\\.xml" . sgml-mode) auto-mode-alist))
;; rename a tag and its closing tag at the same time
(require 'rename-sgml-tag)
(add-hook 'sgml-mode-hook
          (lambda ()
            ;; rename tag
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
            ;; forward/backward sexp
            (define-key sgml-mode-map (kbd "C-M-n") 'sgml-skip-tag-forward)
            (define-key sgml-mode-map (kbd "C-M-p") 'sgml-skip-tag-backward)
            ;; turn on spell checker
            (flyspell-prog-mode)
            ;; turn on line numbering
            (linum-mode t)
            ;; turn on fill-column indicator
            (fci-mode t)
            ;; indentation guide
            (highlight-indentation-mode t)
            ;; turn on hs-minor-mode for folding
            (hs-minor-mode 1)
            ))

(add-to-list 'hs-special-modes-alist
             '(sgml-mode
               "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
               ""
               "<!--"
               sgml-skip-tag-forward
               nil))

;;; Mozilla REPL, js2-mode and etc.
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(require 'moz)
(require 'json)
(require 'js2-mode)

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(add-hook 'js2-mode-hook 'javascript-custom-setup)

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

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; nodejs
(require 'js-comint)
(setq inferior-js-program-command "nodejs")
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string
                        ".*1G\.\.\..*5G" "..."
                        (replace-regexp-in-string
                         ".*1G.*3G" ">>> " output))))))
;;----------------------------------------------------------
;; ---- END web development ----
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
;; ---- BEGIN Android ---
;;----------------------------------------------------------

;; android-mode
(add-to-list 'load-path "~/.emacs.d/dotEmacs/android-mode")
(require 'android-mode)
(setq android-mode-sdk-dir "~/android")

;;----------------------------------------------------------
;; ---- END Android ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN OpenERP ---
;;----------------------------------------------------------
(add-to-list 'load-path "~/Dropbox/hack/openerp-mode")
(require 'openerp-mode)

(setq openerp-server-path "~/openerp/server/")
(setq openerp-conf-path "~/openerp/conf/")

;;----------------------------------------------------------
;; ---- END OpenERP ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Database Management ---
;;----------------------------------------------------------
(require 'edbi)
;;----------------------------------------------------------
;; ---- END Database Management ---
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN Email client ----
;;----------------------------------------------------------
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5)
  )
(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read
          ;; messages
          (lambda ()
            (local-set-key "o" 'my-gnus-group-list-subscribed-groups ))
          )
;;----------------------------------------------------------
;; ---- END Email client ----
;;----------------------------------------------------------



;;----------------------------------------------------------
;; ---- BEGIN default directory and color theme ---
;;----------------------------------------------------------

;; default directory
(cd "~/Dropbox" )

;; (color-theme-subtle-hacker)
(load-theme 'tango-dark)
;; (load-theme 'zenburn t nil)

;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'highlight "#222")
(set-face-foreground 'highlight nil)

;;----------------------------------------------------------
;; ---- END default directory and color theme ---
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
;; ---- END MISC ---
;;----------------------------------------------------------

;; Work environment at WillowIT
;; (setenv "PYTHONPATH" "/home/openerp/openerp/addons:/home/openerp/openerp/server:/home/openerp/openerp/web/addons:/home/openerp/code/plyer:/home/openerp/.emacs.d/el-get/rope/:/home/openerp/kivy/kivy")
