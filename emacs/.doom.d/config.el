;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Cooper Bell"
      user-mail-address "john.cooper.bell@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There Are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq enable-local-variables :all)

;; Set "jj" as the exit chord
(require 'key-chord)
(key-chord-mode t)
(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)

;; Fix "error code 2" with ripgrep
(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

;; Add the GHCUP path to $PATH
(add-to-list 'exec-path "/Users/b377114/.ghcup/bin")
(setenv "PATH" (format "%s:%s" "/Users/b377114/.ghcup/bin" (getenv "PATH")))

;; Gitlab

;;; Add some additional push options for GitLab
(transient-append-suffix 'magit-push "-u"
  '(1 "=s" "Skip GitLab pipeline" "--push-option=ci.skip"))

(transient-append-suffix 'magit-push "-u"
  '(1 "=m" "Create GitLab merge request" "--push-option=merge_request.create"))

(defun open-file-directory-in-finder ()
  "Opens the current file directory in finder"
  (interactive)
  (let ((dir (file-name-directory buffer-file-name)))
    (shell-command (concat "open " dir))))

(defun open-mr ()
  "Open a new merge request in gitlab for the current project"
  (interactive)
  (let* ((project-dir (concat "/Users/b377114/src/" (projectile-project-name)))
         (branch-name (string-trim (let ((default-directory project-dir))
                                     (shell-command-to-string "git symbolic-ref --short HEAD")))))
    (shell-command (concat "open https://gitlab.com/heb-engineering/teams/enterprise/sco/spur/"
                           (projectile-project-name)
                           "/-/merge_requests/new\\?merge_request%5Bsource_branch%5D\\="
                           branch-name))))

(defun open-line-in-gl ()
  "Open the line of the current buffer in gitlab"
  (interactive)
  (let* ((project-dir (concat "/Users/b377114/src/" (projectile-project-name)))
         (branch-name (string-trim (let ((default-directory project-dir))
                                     (shell-command-to-string "git symbolic-ref --short HEAD"))))
         (file-name (substring (buffer-file-name) (length project-dir))))
    (shell-command (concat "open https://gitlab.com/heb-engineering/teams/enterprise/sco/spur/"
                           (projectile-project-name)
                           "/-/blob/"
                           branch-name
                           file-name
                           "#L"
                           (number-to-string
                            (line-number-at-pos))))))

(defun align-comments (beginning end)
  "Align comments within marked region."
  (interactive "*r")
  (align-regexp beginning end (concat "\\(\\s-*\\)"
                                      (regexp-quote comment-start))))


;; PG

(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))

(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

(setq sql-connection-alist
      '((prod
         (sql-product 'postgres)
         (sql-port 5436)
         (sql-server "localhost")
         (sql-user "spurdbuser")
         (sql-password (getenv "KPS_PSPUR_DB_PASS"))
         (sql-database "my-app"))

        (uat
         (sql-product 'postgres)
         (sql-port 5434)
         (sql-server "localhost")
         (sql-user "spurdbuser")
         (sql-password (getenv "KPS_USPUR_DB_PASS"))
         (sql-database "uspur-db"))

        (dev
         (sql-product 'postgres)
         (sql-port 5432)
         (sql-server "localhost")
         (sql-user "spurdbuser")
         (sql-password "pass")
         (sql-database "spurdb"))))

(global-evil-surround-mode 1)

;; Enable tide/typescript-mode for tsx files
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; tree-sitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (sql "https://github.com/m-novikov/tree-sitter-sql")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((dockerfile-mode . dockerfile-ts-mode)
        ;; (sql-mode . sql-ts-mode)
        (json-mode . json-ts-mode)
        (bash-mode . bash-ts-mode)
        (yaml-mode . yaml-ts-mode)))
