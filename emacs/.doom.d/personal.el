;; Projectile won't show all files if this is set to t
(setq projectile-enable-caching nil)

(require 'key-chord)
(key-chord-mode t)
(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)

;;; Use typescript-mode with tsx files
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;;; Javascript indentation
(setq-default js2-basic-offset 2
              js-indent-level 2
              typescript-indent-level 2)

(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "pg_format" nil t)))

(defun get-psql-pass (user db)
  (shell-command-to-string
   (concat "aws s3 cp s3://secrets-vix-prod/rds/us-east-1/" db "/" user " -")))

(defun get-psql-pass-ue2 (user db)
  (shell-command-to-string
   (concat "aws s3 cp s3://secrets-vix-ue2/rds/us-east-2/" db "/" user " -")))

(defun set-db-pass ()
  (interactive)
  (let ((db (read-from-minibuffer "DB: " "singularity"))
        (user (read-from-minibuffer "user: " "vix_admin")))
    (setenv "PGPASSWORD" (get-psql-pass user db))))

(defun set-db-pass-ue2 ()
  (interactive)
  (let ((db (read-from-minibuffer "DB: " "singularity"))
        (user (read-from-minibuffer "user: " "vix_admin")))
    (setenv "PGPASSWORD" (get-psql-pass-ue2 user db))))

(setq sql-postgres-login-params
      '((user :default "vix_admin")
        (database :default "singularity")
        (server :default "localhost")
        (port :default 5436)))

(defun refactor-to-ts-function (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((fun-name (read-from-minibuffer "new function name: ")))
    (goto-char (point-max))
    (insert (concat "\nfunction " fun-name "() {\n"))
    (insert (buffer-substring-no-properties beg end))
    (insert "}")))


(defun open-pr ()
  "Open a new pull request in github for the current project
(assumes the branch is in the form of VER-XXXX)"
  (interactive)
  (let* ((project-dir (concat "/Users/jcbell/src/" (projectile-project-name)))
         (branch-name (string-trim (let ((default-directory project-dir))
                                     (shell-command-to-string "git symbolic-ref --short HEAD")))))
    (shell-command (concat "open https://github.com/VeracityInnovations/"
                           (projectile-project-name)
                           "/pull/new/"
                           branch-name))))

(defun open-jira-card ()
  "Opens the JIRA card that relates to a branch (assumes the branch is in the form of VER-XXXX)"
  (interactive)
  (let* ((project-dir (concat "/Users/jcbell/src/" (projectile-project-name)))
         (branch-name (string-trim (let ((default-directory project-dir))
                                     (shell-command-to-string "git symbolic-ref --short HEAD")))))
    (shell-command (concat "open https://czmdigitalinnovations.atlassian.net/browse/"
                           branch-name))))

;;; Org mode
(setq org-journal-dir "~/org/journal.org")
(setq org-todo-dir "~/org/todo.org")
(setq org-default-notes-file "~/org/notes.org")

(setq org-capture-templates
      '(
        ("t" "Todo"
         entry (file+headline org-todo-dir "Tasks")
         "* TODO %?\n %i\n %a")
        ("j" "Journal"
         entry (file+datetree org-journal-dir)
         "* %?")
        ))

;;; evil-surround
(global-evil-surround-mode 1)

;;; Leader key for evil-lisp-state
;; (evil-lisp-state-leader ", l")

;;; Helm-ag
(setq helm-ag-default-args (string-join
                            '("--ignore node_modules"
                              "--ignore dist")
                            " "))

(defun helm-ag-ts-only ()
  (interactive)
  (setq helm-ag-command-option (concat "-C 5 " " --ts " helm-ag-default-args)))

(defun helm-ag-with-context ()
  (interactive)
  (setq helm-ag-command-option (concat "-C 5 " helm-ag-default-args)))

(defun helm-ag-with-no-context ()
  (interactive)
  (setq helm-ag-command-option helm-ag-default-args))

;; Drag stuff using Vim home row
;; (global-set-key (kbd "C-j") 'drag-stuff-down)
(global-set-key (kbd "C-j") 'move-down)
(global-set-key (kbd "C-k") 'drag-stuff-up)

;; Haskell
(map! :leader
      :mode 'haskell-interactive-mode
      "m `" #'haskell-interactive-bring)

(map! :leader
      :mode 'haskell-interactive-mode
      "m l" #'haskell-process-load-file)

;; Rust
(setq lsp-rust-server 'rust-analyzer)

;;; Common lisp
;; (setq inferior-lisp-program /usr/local/bin/ccl64")
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
;; (slime-setup '(slime-fancy slime-company))

(custom-set-variables
 '(haskell-stylish-on-save t))

;;; Default geiser scheme
(setq geiser-active-implementations '(chez))

(setq python-shell-interpreter "/usr/local/bin/python3")

;; (exec-path-from-shell-copy-env "AWS_PROFILE")
;; (exec-path-from-shell-copy-env "AWS_REGION")
;; (exec-path-from-shell-copy-env "VIX_API_URL")

(defun camel-to-snake ()
  (interactive)
  (cond
   ((region-active-p) (let ((new-str
                             (string-camel-to-snake
                              (buffer-substring
                               (region-beginning)
                               (region-end)))))
                        (delete-active-region)
                        (insert new-str)))))

(defun string-camel-to-snake (str)
  "Convert a camel string to a snake string"
  (setq case-fold-search nil)
  (let ((pos 0)
        matches)
    (while (string-match "\\([A-Z]?[^A-Z]+\\)" str pos)
      (push (downcase (match-string 0 str)) matches)
      (setq pos (match-end 0)))
    (apply #'concat (intercalate "_" (reverse matches)))))

(defun intercalate (e lst)
  (cdr
   (apply #'append
          (mapcar (lambda (c) (list e c)) lst))))
