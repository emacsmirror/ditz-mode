;;; ditz-mode.el --- Emacs interface to Ditz issue tracking system
;;
;; Copyright (C) 2010 Kentaro Kuribayashi, Glenn Hutchings
;;
;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;;         Glenn Hutchings <zondo42@googlemail.com>
;; Maintainer: Glenn Hutchings <zondo42@googlemail.com>
;; Keywords: tools
;; Version: 0.1
;;
;; This file is not a part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is an Emacs interface to the Ditz distributed issue tracking
;; system, which can be found at http://ditz.rubyforge.org.
;;
;; Put this file in your Lisp load path and something like the following in
;; your .emacs file:
;;
;;     (require 'ditz-mode)
;;     (define-key global-map "\C-c\C-d" 'ditz-todo)
;;
;; See the documentation for `ditz-mode' for more info.
;;
;;; History:
;;
;; Version 0.1 (17 Mar 2010):
;;    First version.
;;
;;; Code:

;;;; Customizable stuff.

(defgroup ditz nil
  "Interface to Ditz distributed bug tracker."
  :prefix "ditz-"
  :group 'tools)

(defcustom ditz-program "ditz"
  "Ditz command."
  :type 'string
  :group 'ditz)

;;;; Constants.

(defconst ditz-config-filename ".ditz-config"
  "File name of the Ditz config file.")

(defconst ditz-issue-id-regex "\\([a-z][a-z-]*-[0-9]+\\)"
  "Regex for issue id.")

(defconst ditz-issue-attr-regex
  (concat (regexp-opt '("Title" "Description" "Type" "Status" "Creator"
			"Age" "Release" "References" "Identifier") 'words) ":")
  "Regex for issue attribute.")

(defconst ditz-issue-status-regex
  (concat (regexp-opt '("unstarted" "in_progress" "fixed" "wontfix"
			"reorg") 'words) ":")
  "Regex for issue status.")

(defconst ditz-log-attr-regex
  (concat (regexp-opt '("date" "author" "issue") 'words) " *:")
  "Regex for log attribute.")

(defconst ditz-release-name-regex "^\\([^ ]+\\) (.*$"
  "Regex for release name.")

(defconst ditz-comment-regex "^\s+\\(>.*\\)$"
  "Regex for comment.")

(defconst ditz-feature-regex "(\\(feature\\))"
  "Regex for feature indicator.")

(defconst ditz-bug-regex "(\\(bug\\))"
  "Regex for bug indicator.")

;;;; Variables.

(defvar ditz-todo-flags ""
  "Flags to pass to `ditz-todo'.")

(defvar ditz-todo-release ""
  "Release specified by `ditz-todo'.")

;;;; Commands.

(defun ditz-todo ()
  "Show current todo list."
  (interactive)
  (ditz-call-process "todo" (ditz-todo-args) "switch"))

(defun ditz-add ()
  "Add a new issue."
  (interactive)
  (ditz-call-process "add" nil "pop" t))

(defun ditz-add-release ()
  "Add a new release."
  (interactive)
  (ditz-call-process "add-release" nil "pop" t))

(defun ditz-status ()
  "Show status of issues."
  (interactive)
  (ditz-call-process "status" nil "display"))

(defun ditz-log ()
  "Show log of recent activities."
  (interactive)
  (ditz-call-process "log" nil "pop"))

(defun ditz-shortlog ()
  "Show short log of recent activities."
  (interactive)
  (ditz-call-process "shortlog" nil "pop"))

(defun ditz-show ()
  "Show issue details."
  (interactive)
  (ditz-call-process "show" (ditz-extract-issue) "switch"))

(defun ditz-show-other-window ()
  "Show issue details in another window."
  (interactive)
  (let ((issue-id (ditz-extract-issue t)))
    (when issue-id
      (ditz-call-process "show" issue-id "display-other"))))

(defun ditz-grep (regexp)
  "Show issue details."
  (interactive "sShow issues matching regexp: ")
  (ditz-call-process "grep" regexp "pop"))

(defun ditz-assign ()
  "Assign issue to a release."
  (interactive)
  (ditz-call-process "assign" (ditz-extract-issue) "switch" t))

(defun ditz-unassign ()
  "Unassign an issue."
  (interactive)
  (ditz-call-process "unassign" (ditz-extract-issue) "switch" t))

(defun ditz-comment ()
  "Comment on an issue."
  (interactive)
  (ditz-call-process "comment" (ditz-extract-issue) "pop" t))

(defun ditz-edit ()
  "Edit issue details."
  (interactive)
  (let ((issue-id (ditz-extract-issue))
	(issue-dir (ditz-issue-directory)))
    (ditz-call-process "show" issue-id)
    (goto-char (point-min))
    (save-excursion
      (let ((beg (search-forward "Identifier: "))
	    (end (line-end-position)))
	(setq issue-id (buffer-substring-no-properties beg end))))
    (find-file (ditz-issue-file issue-id))))

(defun ditz-edit-project ()
  "Edit the project file."
  (interactive)
  (find-file (ditz-project-file)))

(defun ditz-close ()
  "Close an issue."
  (interactive)
  (ditz-call-process "close" (ditz-extract-issue) "switch" t))

(defun ditz-drop ()
  "Drop an issue."
  (interactive)
  (let ((issue-id (ditz-extract-issue)))
    (when (yes-or-no-p (concat "Drop " issue-id " "))
      (ditz-call-process "drop" issue-id "switch"))))

(defun ditz-start ()
  "Start work on an issue."
  (interactive)
  (ditz-call-process "start" (ditz-extract-issue) "switch" t))

(defun ditz-stop ()
  "Stop work on an issue."
  (interactive)
  (ditz-call-process "stop" (ditz-extract-issue) "switch" t))

(defun ditz-set-component ()
  "Set an issue's component."
  (interactive)
  (ditz-call-process "set-component" (ditz-extract-issue) "switch" t))

(defun ditz-add-reference ()
  "Add an issue reference."
  (interactive)
  (ditz-call-process "add-reference" (ditz-extract-issue) "switch" t))

(defun ditz-release ()
  "Mark release as released."
  (interactive)
  (let ((release-name (ditz-extract-release)))
    (ditz-call-process "release" release-name "switch" t)))

(defun ditz-toggle-status ()
  "Show/hide by issue status."
  (interactive)
  (if (string= ditz-todo-flags "")
      (setq ditz-todo-flags "-a")
    (setq ditz-todo-flags ""))
  (ditz-toggle-message)
  (ditz-reload))

(defun ditz-toggle-release ()
  "Show/hide by release."
  (interactive)
  (if (string= ditz-todo-release "")
      (setq ditz-todo-release (ditz-extract-release))
    (setq ditz-todo-release ""))
  (ditz-toggle-message)
  (ditz-reload))

(defun ditz-next-line ()
  "Go to the next line, showing the issue in another window."
  (interactive)
  (next-line)
  (ditz-show-other-window))

(defun ditz-previous-line ()
  "Go to the previous line, showing the issue in another window."
  (interactive)
  (previous-line)
  (ditz-show-other-window))

(defun ditz-html-generate ()
  "Generate HTML files of issues, returning the created HTML file."
  (interactive)
  (ditz-call-process "html" nil)
  (when (search-forward "URL: " nil t)
    (let ((url (buffer-substring (point) (line-end-position))))
      (message "Generated HTML in %s" url)
      url)))

(defun ditz-html-browse ()
  "Generate and browse HTML files of issues."
  (interactive)
  (browse-url-of-file (ditz-html-generate)))

(defun ditz-archive ()
  "Archive a release."
  (interactive)
  (let ((release-name (ditz-extract-release)))
    (when (yes-or-no-p (concat "Archive release " release-name "? "))
      (ditz-call-process "archive" release-name "display")
      (ditz-reload))))

(defun ditz-changelog ()
  "Show change log for a release."
  (interactive)
  (let ((release-name (ditz-extract-release)))
    (ditz-call-process "changelog" release-name "display")))

(defun ditz-reload ()
  "Reload the current Ditz buffer."
  (interactive)
  (goto-char (point-min))
  (cond ((string= (buffer-name) "*ditz-todo*")
         (ditz-call-process "todo" (ditz-todo-args) "switch"))
        ((string= (buffer-name) "*ditz-status*")
         (ditz-call-process "status" nil "switch"))
        ((string= (buffer-name) "*ditz-show*")
         (ditz-call-process "show" (ditz-extract-issue) "switch"))
        ((string= (buffer-name) "*ditz-shortlog*")
	 (ditz-call-process "shortlog" nil "switch"))
        ((string= (buffer-name) "*ditz-log*")
         (ditz-call-process "log" nil "switch"))))

(defun ditz-quit ()
  "Bury current Ditz buffer."
  (interactive)
  (bury-buffer))

(defun ditz-quit-all ()
  "Bury all Ditz buffers."
  (interactive)
  (delete-other-windows)
  (dolist (name '("todo" "status" "show" "shortlog" "log" "grep"))
    (let ((buffer (get-buffer (concat "*ditz-" name "*"))))
      (when buffer
	(with-current-buffer buffer
	  (bury-buffer (current-buffer))
	  (replace-buffer-in-windows))))))

;;;; Internal functions.

(defun ditz-todo-args ()
  "Return current ditz todo arguments."
  (format "%s %s" ditz-todo-flags ditz-todo-release))

(defun ditz-toggle-message ()
  "Give message based on current display settings."
  (message
   (concat "Showing "
	   (if (string= ditz-todo-flags "")
	       "unresolved" "all")
	   " issues "
	   (if (string= ditz-todo-release "")
	       "" (format "assigned to release %s" ditz-todo-release)))))

(defun ditz-extract-issue (&optional noerror)
  "Extract an issue ID from the current line."
  (let ((issue-id (ditz-extract-thing-at-point ditz-issue-id-regex 1)))
    (unless (or issue-id noerror)
      (error "No issue on this line"))
    issue-id))

(defun ditz-extract-release (&optional noerror)
  "Extract a release ID from the current line."
  (let ((release (ditz-extract-thing-at-point ditz-release-name-regex 1)))
    (unless (or release noerror)
      (error "No release on this line"))
    release))

(defun ditz-extract-thing-at-point (regex n)
  "Extract REGEX at the current point."
  (save-excursion
    (let ((line (buffer-substring-no-properties (progn
						  (beginning-of-line)
						  (point))
						(progn
						  (end-of-line)
						  (point)))))
      (when (string-match regex line)
        (match-string n line)))))

(defun ditz-call-process (command &optional arg popup-flag interactive)
  "Invoke a Ditz command."

  (let* ((issuedir (ditz-issue-directory))
	 (quoteddir (concat "\"" issuedir "\""))
	 (cmd (mapconcat 'identity
			 (list ditz-program "-i" quoteddir command arg) " "))
	 (bufname (concat "*ditz-" command "*"))
	 (buffer (get-buffer-create bufname))
         (proc (get-buffer-process buffer)))

    ;; If not interactive, make sure we get a new buffer.
    (unless interactive
      (kill-buffer buffer)
      (setq buffer (get-buffer-create bufname)))

    ;; If interactive and already running, ask for confirmation.
    (if (and interactive proc (eq (process-status proc) 'run))
        (when (y-or-n-p (format "A %s process is running; kill it? "
                                (process-name proc)))
          (interrupt-process proc)
          (sit-for 1)
          (delete-process proc)))

    ;; Initialize command buffer.
    (with-current-buffer buffer
      (setq default-directory (file-name-directory issuedir))
      (setq buffer-read-only nil)
      (erase-buffer)
      (buffer-disable-undo (current-buffer)))

    ;; Start the Ditz process.
    (if interactive
	(make-comint-in-buffer "ditz-call-process"
			       buffer shell-file-name nil
			       shell-command-switch cmd)
      (call-process-shell-command cmd nil buffer))

    ;; Display results.
    (cond ((string= popup-flag "switch")
	   (switch-to-buffer buffer))
          ((string= popup-flag "pop")
           (pop-to-buffer buffer))
          ((string= popup-flag "display")
           (display-buffer buffer))
          ((string= popup-flag "display-other")
	   (with-current-buffer buffer
	     (ditz-mode)
	     (goto-char (point-min)))
           (display-buffer buffer))
          (t
           (set-buffer buffer)))


    ;; Go to Ditz mode if required.
    (when (and (not interactive) (eq buffer (current-buffer)))
      (ditz-mode)
      (goto-char (point-min)))))

(defun ditz-project-file ()
  "Return the pathname of the current project file."
  (concat (ditz-issue-directory) "/project.yaml"))

(defun ditz-issue-file (id)
  "Return the pathname of issue ID."
  (concat (ditz-issue-directory) "/issue-" id ".yaml"))

(defun ditz-issue-directory ()
  "Return pathname of the Ditz issue directory.

The issue directory is found by searching upward from the current
directory for a .ditz-config file containing the issue_dir
setting.  Then the issue directory is located either in the
current directory or the one with the .ditz-config file in it."

  (let* ((configfile (ditz-find-config))
	 (parentdir (file-name-directory configfile))
	 (buf (get-buffer-create "*ditz-config*"))
	 (issuedir nil)
	 (issuedirname nil))

    ;; Get issue directory name from config file.
    (with-current-buffer buf
      (insert-file-contents-literally configfile nil nil nil t)
      (goto-char (point-min))
      (if (search-forward "issue_dir: ")
	  (setq issuedirname (buffer-substring (point) (line-end-position)))
	(error "Can't find 'issue_dir' setting in %s" configfile)))

    ;; Try looking in current directory for it.
    (setq issuedir (concat default-directory "/" issuedirname))

    ;; If not there, try same directory as config file.
    (unless (file-exists-p issuedir)
      (setq issuedir (concat parentdir "/" issuedirname)))

    ;; If still not there, give up.
    (unless (file-exists-p issuedir)
      (error "Can't find Ditz issue directory '%s'" issuedirname))

    (expand-file-name issuedir)))

(defun ditz-find-config ()
  "Find Ditz config file in current or parent directories."

  (let ((curdir (expand-file-name (file-name-directory default-directory)))
	(configfile nil))

    (while (not configfile)
      (setq path (concat curdir "/" ditz-config-filename))
      (cond ((file-exists-p path)
	     (setq configfile path))
	    ((string= curdir "/")
	     (error "Can't find %s; have you run 'ditz init'?"
		    ditz-config-filename))
	    (t
	     (setq curdir (directory-file-name (file-name-directory curdir))))))
    configfile))

;;;; Hooks.

(defvar ditz-mode-hook nil
  "*Hooks for Ditz major mode.")

;;;; Commands.

;; Main commands.
(defvar ditz-mode-map (make-keymap)
  "*Keymap for Ditz major mode.")

(define-key ditz-mode-map " " 'ditz-show)
(define-key ditz-mode-map "l" 'ditz-shortlog)
(define-key ditz-mode-map "L" 'ditz-log)
(define-key ditz-mode-map "/" 'ditz-grep)
(define-key ditz-mode-map "n" 'ditz-next-line)
(define-key ditz-mode-map "p" 'ditz-previous-line)
(define-key ditz-mode-map "g" 'ditz-reload)
(define-key ditz-mode-map "q" 'ditz-quit)
(define-key ditz-mode-map "Q" 'ditz-quit-all)
(define-key ditz-mode-map "?" 'describe-mode)

;; Issue commands.
(defvar ditz-issue-mode-map (make-keymap)
  "*Keymap for Ditz issue commands.")

(define-key ditz-mode-map "i" ditz-issue-mode-map)

(define-key ditz-issue-mode-map "n" 'ditz-add)
(define-key ditz-issue-mode-map "c" 'ditz-comment)
(define-key ditz-issue-mode-map "<" 'ditz-start)
(define-key ditz-issue-mode-map ">" 'ditz-stop)
(define-key ditz-issue-mode-map "a" 'ditz-assign)
(define-key ditz-issue-mode-map "u" 'ditz-unassign)
(define-key ditz-issue-mode-map "r" 'ditz-add-reference)
(define-key ditz-issue-mode-map "o" 'ditz-set-component)
(define-key ditz-issue-mode-map "e" 'ditz-edit)
(define-key ditz-issue-mode-map "C" 'ditz-close)
(define-key ditz-issue-mode-map "D" 'ditz-drop)

;; Release commands.
(defvar ditz-release-mode-map (make-keymap)
  "*Keymap for Ditz release commands.")

(define-key ditz-mode-map "r" ditz-release-mode-map)

(define-key ditz-release-mode-map "n" 'ditz-add-release)
(define-key ditz-release-mode-map "e" 'ditz-edit-project)
(define-key ditz-release-mode-map "r" 'ditz-release)
(define-key ditz-release-mode-map "s" 'ditz-status)
(define-key ditz-release-mode-map "l" 'ditz-changelog)
(define-key ditz-release-mode-map "a" 'ditz-archive)

;; Toggle commands.
(defvar ditz-toggle-mode-map (make-keymap)
  "*Keymap for Ditz toggle commands.")

(define-key ditz-mode-map "t" ditz-toggle-mode-map)

(define-key ditz-toggle-mode-map "s" 'ditz-toggle-status)
(define-key ditz-toggle-mode-map "r" 'ditz-toggle-release)

;; HTML commands.
(defvar ditz-html-mode-map (make-keymap)
  "*Keymap for Ditz HTML commands.")

(define-key ditz-mode-map "h" ditz-html-mode-map)

(define-key ditz-html-mode-map "g" 'ditz-html-generate)
(define-key ditz-html-mode-map "b" 'ditz-html-browse)

;;;; Easymenu.

(easy-menu-define ditz-mode-menu ditz-mode-map "Ditz mode menu"
 '("Ditz"
   ("Display"
    ["Current issue"                    ditz-show t]
    "---"
    ["Next issue"                       ditz-next-line t]
    ["Previous issue"                   ditz-previous-line t]
    "---"
    ["Short log"                        ditz-shortlog t]
    ["Detailed log"                     ditz-log t]
    "---"
    ["Issues matching regexp"           ditz-grep t])

   ("Issue"
    ["New"                              ditz-add t]
    ["Edit"                        	ditz-edit t]
    "---"
    ["Start working"            	ditz-start t]
    ["Stop working"             	ditz-stop t]
    "---"
    ["Add comment"                  	ditz-comment t]
    ["Add reference"            	ditz-add-reference t]
    ["Set component"          		ditz-set-component t]
    "---"
    ["Assign to release"           	ditz-assign t]
    ["Unassign"                    	ditz-unassign t]
    "---"
    ["Close"                       	ditz-close t]
    ["Drop"                        	ditz-drop t])

   ("Release"
    ["New"                              ditz-add-release t]
    ["Edit"                             ditz-edit-project t]
    "---"
    ["Show status"                      ditz-status t]
    ["Show changelog"                   ditz-changelog t]
    "---"
    ["Release"                          ditz-release t]
    ["Archive"                          ditz-archive t])

   ("Show/hide"
    ["By issue status"                  ditz-toggle-status t]
    ["By release"             		ditz-toggle-release t])

   ("HTML"
    ["Generate"                         ditz-html t]
    ["Browse"                           ditz-html-browse t])

   "---"
   ["Refresh"                           ditz-reload t]
   ["Quit"                              ditz-quit t]
   ["Quit all"             		ditz-quit-all t]))

;;;; Faces.

(defface ditz-issue-id-face
  '((((class color) (background light))
     (:foreground "blue" :weight bold))
    (((class color) (background dark))
     (:foreground "blue" :weight bold)))
  "Face definition for issue id."
  :group 'ditz)

(defface ditz-issue-attr-face
  '((((class color) (background light))
     (:foreground "steel blue" :weight bold))
    (((class color) (background dark))
     (:foreground "steel blue" :weight bold)))
  "Face definition for issue attribute."
  :group 'ditz)

(defface ditz-release-name-face
  '((((class color) (background light))
     (:foreground "red" :weight bold))
    (((class color) (background dark))
     (:foreground "red" :weight bold)))
  "Face definition for release name."
  :group 'ditz)

(defface ditz-comment-face
  '((((class color) (background light))
     (:foreground "dim gray" :slant italic))
    (((class color) (background dark))
     (:foreground "dim gray" :slant italic)))
  "Face definition for comments."
  :group 'ditz)

(defface ditz-feature-face
  '((((class color) (background light))
     (:foreground "dark green"))
    (((class color) (background dark))
     (:foreground "dark green")))
  "Face definition for feature indicators."
  :group 'ditz)

(defface ditz-bug-face
  '((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "red")))
  "Face definition for bug indicators."
  :group 'ditz)

(defconst ditz-issue-id-face 'ditz-issue-id-face)
(defconst ditz-issue-attr-face 'ditz-issue-attr-face)
(defconst ditz-release-name-face 'ditz-release-name-face)
(defconst ditz-comment-face 'ditz-comment-face)
(defconst ditz-feature-face 'ditz-feature-face)
(defconst ditz-bug-face 'ditz-bug-face)

(defvar ditz-font-lock-keywords
  `((,ditz-issue-attr-regex (1 ditz-issue-attr-face t))
    (,ditz-log-attr-regex (1 ditz-issue-attr-face t))
    (,ditz-comment-regex (1 ditz-comment-face t))
    (,ditz-issue-id-regex (1 ditz-issue-id-face t))
    (,ditz-release-name-regex (1 ditz-release-name-face t))
    (,ditz-feature-regex (1 ditz-feature-face t))
    (,ditz-bug-regex (1 ditz-bug-face t))))

;;;; Ditz major mode.

(define-derived-mode ditz-mode fundamental-mode "Ditz"
  "Major mode for the Ditz distributed issue tracker.

Ditz mode operates on directories that have already been
Ditz-enabled: i.e., 'ditz init' has already been run from the
command line.  After that, you can invoke 'ditz-todo' and get a
list of issues in a Ditz buffer.

See `ditz-issue-directory' for details on how the Ditz issue
directory is located.

Calling this function invokes the function(s) listed in
`ditz-mode-hook' before doing anything else.

\\{ditz-mode-map}
"
  :group 'ditz

  (interactive)
  (kill-all-local-variables)

  ;; Become the current major mode.
  (setq major-mode 'ditz-mode)
  (setq mode-name "Ditz")

  ;; Set readonly.
  (setq buffer-read-only t)

  ;; Set up font lock.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ditz-font-lock-keywords t))

  ;; Activate keymap.
  (use-local-map ditz-mode-map)

  ;; Run startup hooks.
  (run-hooks 'ditz-mode-hook))

(provide 'ditz-mode)
;;; ditz-mode.el ends here
