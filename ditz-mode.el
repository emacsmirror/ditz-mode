;;; ditz.el --- Emacs interface to Ditz issue tracking system 

;; Copyright (C) 2008 Kentaro Kuribayashi

;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;; Keywords: ditz, todo

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README file or the website below:
;; http://github.com/kentaro/emacs-ditz/tree/master

;;; Code:

(defgroup ditz nil
  "Interface to Ditz distributed bug tracker."
  :prefix "ditz-"
  :group 'tools)

;; Customizable variables.
(defcustom ditz-program "ditz"
  "Ditz command"
  :type 'string
  :group 'ditz)

(defcustom ditz-issue-directory "bugs"
  "Default directory name in which issues are stored.

You must set it some value according with your environment when
you use automatic finding described below."
  :type 'string
  :group 'ditz)

(defcustom ditz-find-issue-directory-automatically-flag t
  "If non-nil, issue directory will be found automatically in
directories from the current one toward the root. Otherwise, you
must set it from minibuffer."
  :type 'boolean
  :group 'ditz)

;; Constant variables.
(defconst ditz-issue-id-regex "\\([a-z0-9]+-[0-9]+\\)"
  "Regex for issue id.")

(defconst ditz-issue-attr-regex
  (concat (regexp-opt '("Title" "Description" "Type" "Status" "Creator"
			"Age" "Release" "References" "Identifier") 'words) ":")
  "Regex for issue attribute.")

(defconst ditz-release-name-regex "^\\([^ ]+\\) (.*$"
  "Regex for release name.")

(defconst ditz-comment-regex "^\s+\\(>.*\\)$"
  "Regex for comment.")

(defconst ditz-feature-regex "(\\(feature\\))"
  "Regex for feature indicator.")

(defconst ditz-bug-regex "(\\(bug\\))"
  "Regex for bug indicator.")

;; Commands.
(defun ditz-init ()
  "Initialize ditz issues."
  (interactive)
  (ditz-call-process "init" nil "pop" t))

(defun ditz-html ()
  "Generate HTML files of issues."
  (interactive)
  (ditz-call-process "html" nil)
  (when (search-forward "URL: " nil t)
    (let ((url (buffer-substring (point) (line-end-position))))
      (message "Generated HTML in %s" url)
      url)))

(defun ditz-html-browse ()
  "Generate and browse HTML files of issues."
  (interactive)
  (browse-url-of-file (ditz-html)))

(defun ditz-add-release ()
  "Add a new release."
  (interactive)
  (ditz-call-process "add-release" nil "pop" t))

(defun ditz-add ()
  "Add a new issue."
  (interactive)
  (ditz-call-process "add" nil "pop" t))

(defun ditz-status ()
  "Show status of issues."
  (interactive)
  (ditz-call-process "status" nil "display"))

(defun ditz-todo ()
  "Show current todo."
  (interactive)
  (ditz-call-process "todo" nil "switch"))

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
      (ditz-call-process "show" issue-id "display-ditz"))))

(defun ditz-grep (regexp)
  "Show issue details."
  (interactive "sShow issues matching regexp: ")
  (ditz-call-process "grep" regexp "switch"))

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

(defun ditz-release ()
  "Mark release as released."
  (interactive)
  (let ((release-name (ditz-extract-release)))
    (ditz-call-process "release" release-name "switch" t)))

(defun ditz-archive ()
  "Archive a release."
  (interactive)
  (let ((release-name (ditz-extract-release)))
    (when (yes-or-no-p (concat "Archive " release-name " "))
      (ditz-call-process "archive" release-name "display")
      (ditz-reload))))

(defun ditz-changelog ()
  "Show change log for a release."
  (interactive)
  (let ((release-name (ditz-extract-release)))
    (ditz-call-process "changelog" release-name "display")))

(defun ditz-extract-issue (&optional noerror)
  (let ((issue-id (ditz-extract-thing-at-point ditz-issue-id-regex 1)))
    (unless (or issue-id noerror)
      (error "No issue on this line"))
    issue-id))

(defun ditz-extract-release (&optional noerror)
  (let ((release (ditz-extract-thing-at-point ditz-release-name-regex 1)))
    (unless (or release noerror)
      (error "No release on this line"))
    release))

(defun ditz-extract-thing-at-point (regex n)
  (save-excursion
    (let ((line (buffer-substring-no-properties (progn
						  (beginning-of-line)
						  (point))
						(progn
						  (end-of-line)
						  (point)))))
      (when (string-match regex line)
        (match-string n line)))))

(defun ditz-next-line ()
  (interactive)
  (next-line)
  (ditz-show-other-window))

(defun ditz-previous-line ()
  (interactive)
  (previous-line)
  (ditz-show-other-window))

(defun ditz-reload ()
  (interactive)
  (goto-char (point-min))
  (cond ((string= (buffer-name) "*ditz-todo*")
         (ditz-call-process "todo" nil "switch"))
        ((string= (buffer-name) "*ditz-status*")
         (ditz-call-process "status" nil "switch"))
        ((string= (buffer-name) "*ditz-show*")
         (ditz-call-process "show" (ditz-extract-issue) "switch"))
        ((string= (buffer-name) "*ditz-shortlog*")
	 (ditz-call-process "shortlog" nil "switch"))
        ((string= (buffer-name) "*ditz-log*")
         (ditz-call-process "log" nil "switch"))))

(defun ditz-quit ()
  "Bury all Ditz buffers."
  (interactive)
  (dolist (name '("todo" "status" "show" "shortlog" "log"))
    (with-current-buffer (concat "*ditz-" name "*")
      (bury-buffer (current-buffer))
      (replace-buffer-in-windows))))

(defun ditz-call-process (command &optional arg popup-flag interactive)
  "Invoke a ditz command."

  (let* ((cmd (ditz-build-command command arg))
	 (bufname (concat "*ditz-" command "*"))
	 (buffer (get-buffer-create bufname))
         (proc (get-buffer-process buffer)))

    (unless interactive
      (kill-buffer buffer)
      (setq buffer (get-buffer-create bufname)))

    (if (and interactive proc (eq (process-status proc) 'run))
        (when (y-or-n-p (format "A %s process is running; kill it?"
                                (process-name proc)))
          (interrupt-process proc)
          (sit-for 1)
          (delete-process proc)))

    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (buffer-disable-undo (current-buffer)))

    (if interactive
	(make-comint-in-buffer "ditz-call-process"
			       buffer shell-file-name nil
			       shell-command-switch cmd)
      (call-process-shell-command cmd nil buffer))

    (cond ((string= popup-flag "switch")
	   (switch-to-buffer buffer))
          ((string= popup-flag "pop")
           (pop-to-buffer buffer))
          ((string= popup-flag "display")
           (display-buffer buffer))
          ((string= popup-flag "display-ditz")
	   (with-current-buffer buffer
	     (ditz-mode)
	     (goto-char (point-min)))
           (display-buffer buffer))
          (t
           (set-buffer buffer)))

    (when (and (not interactive) (eq buffer (current-buffer)))
      (ditz-mode)
      (goto-char (point-min)))))

(defvar ditz-last-visited-issue-directory nil)

(defun ditz-project-file ()
  (concat (ditz-issue-directory) "/project.yaml"))

(defun ditz-issue-file (id)
  (concat (ditz-issue-directory) "/issue-" id ".yaml"))

(defun ditz-issue-directory (&optional command)
  (let (issue-directory current-directory)

    ;; Reserve current directory to come back later.  It's needed when
    ;; automatically finding directory.
    (when buffer-file-name
      (setq current-directory (file-name-directory (buffer-file-name))))

    (cond ((eq major-mode 'ditz-mode)
           (setq issue-directory ditz-last-visited-issue-directory))
          ((and (not (string= command "init"))
                ditz-find-issue-directory-automatically-flag
                (catch 'loop
                  (while t
                    (cond ((file-exists-p ditz-issue-directory)
                           (throw 'loop t))
                          ((string= "/" default-directory)
                           (throw 'loop nil))
                          (t
                           (cd ".."))))))
           (setq issue-directory
		 (concat default-directory ditz-issue-directory)))
          (t
           (setq issue-directory
                 (read-file-name "Issue dir: "
                                 (or ditz-last-visited-issue-directory
                                     default-directory)))))

    ;; Restore default directory if needed.
    (when current-directory
      (setq default-directory current-directory))

    issue-directory))

(defun ditz-build-command (command arg)
  (let ((issue-directory (ditz-issue-directory command)))
    (setq ditz-last-visited-issue-directory issue-directory)
    (mapconcat 'identity
               (list ditz-program "-i" issue-directory command arg) " ")))

;; Hooks.
(defvar ditz-mode-hook nil
  "*Hooks for Ditz major mode")

;; Keymap.
(defvar ditz-mode-map (make-keymap)
  "*Keymap for Ditz major mode")

(define-key ditz-mode-map " " 'ditz-show)
(define-key ditz-mode-map "l" 'ditz-shortlog)
(define-key ditz-mode-map "L" 'ditz-log)
(define-key ditz-mode-map "/" 'ditz-grep)

(define-key ditz-mode-map "A" 'ditz-add)
(define-key ditz-mode-map "c" 'ditz-comment)
(define-key ditz-mode-map "<" 'ditz-start)
(define-key ditz-mode-map ">" 'ditz-stop)
(define-key ditz-mode-map "O" 'ditz-set-component)
(define-key ditz-mode-map "E" 'ditz-edit)
(define-key ditz-mode-map "C" 'ditz-close)
(define-key ditz-mode-map "D" 'ditz-drop)

(define-key ditz-mode-map "r" 'ditz-add-release)
(define-key ditz-mode-map "a" 'ditz-assign)
(define-key ditz-mode-map "u" 'ditz-unassign)
(define-key ditz-mode-map "R" 'ditz-release)
(define-key ditz-mode-map "S" 'ditz-status)
(define-key ditz-mode-map "G" 'ditz-changelog)
(define-key ditz-mode-map "$" 'ditz-archive)

(define-key ditz-mode-map "H" 'ditz-html)
(define-key ditz-mode-map "B" 'ditz-html-browse)

(define-key ditz-mode-map "g" 'ditz-reload)
(define-key ditz-mode-map "q" 'ditz-quit)

(define-key ditz-mode-map "n" 'ditz-next-line)
(define-key ditz-mode-map "p" 'ditz-previous-line)

(define-key ditz-mode-map "h" 'describe-mode)

;; Easymenu.
(easy-menu-define ditz-mode-menu ditz-mode-map "Ditz mode menu"
 '("Ditz"
   ["Show issue details"                ditz-show t]
   ["Show short log"                    ditz-shortlog t]
   ["Show detailed log"                 ditz-log t]
   ["Show issues matching regexp"       ditz-grep t]
   "---"
   ["Add new issue"                     ditz-add t]
   ["Comment on issue"                  ditz-comment t]
   ["Start working on issue"            ditz-start t]
   ["Stop working on issue"             ditz-stop t]
   ["Set an issue's component"          ditz-set-component t]
   ["Edit issue"                        ditz-edit t]
   ["Close issue"                       ditz-close t]
   ["Drop issue"                        ditz-drop t]
   "---"
   ["Add new release"                   ditz-add-release t]
   ["Assign issue to release"           ditz-assign t]
   ["Unassign issue"                    ditz-unassign t]
   ["Release version"                   ditz-release t]
   ["Show release status"               ditz-status t]
   ["Show release changelog"            ditz-changelog t]
   ["Archive a release"                 ditz-archive t]
   "---"
   ["Generate HTML summary"             ditz-html t]
   ["Browse HTML summary"               ditz-html-browse t]
   "---"
   ["Refresh buffer"                    ditz-reload t]
   ["Quit"                              ditz-quit t]))

;; Faces.
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
     (:foreground "dim gray"))
    (((class color) (background dark))
     (:foreground "dim gray")))
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

(defconst ditz-font-lock-keywords
  `((,ditz-issue-attr-regex (1 ditz-issue-attr-face t))
    (,ditz-issue-id-regex (1 ditz-issue-id-face t))
    (,ditz-release-name-regex (1 ditz-release-name-face t))
    (,ditz-comment-regex (1 ditz-comment-face t))
    (,ditz-feature-regex (1 ditz-feature-face t))
    (,ditz-bug-regex (1 ditz-bug-face t))))

;; Ditz major mode.
(define-derived-mode ditz-mode fundamental-mode "Ditz"
  "Major mode for the Ditz distributed issue tracker.

\\{ditz-mode-map}

Calling this function invokes the function(s) listed in `ditz-mode-hook'
before doing anything else."
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

(provide 'ditz)
;;; ditz.el ends here
