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
(require 'key-chord)
(key-chord-mode t)
(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun open-pr ()
  "Open a new pull request in github for the current project"
  (interactive)
  (let* ((project-dir (concat "/Users/jcbell/src/" (projectile-project-name)))
         (branch-name (string-trim (let ((default-directory project-dir))
                                     (shell-command-to-string "git symbolic-ref --short HEAD")))))
    (shell-command (concat "open https://github.com/VeracityInnovations/"
                           (projectile-project-name)
                           "/pull/new/"
                           branch-name))))

(defun open-line-in-gh ()
  "Open the line of the current buffer in github"
  (interactive)
  (let* ((project-dir (concat "/Users/jcbell/src/" (projectile-project-name)))
         (branch-name (string-trim (let ((default-directory project-dir))
                                     (shell-command-to-string "git symbolic-ref --short HEAD"))))
         (file-name (substring (buffer-file-name) (length project-dir))))
    (shell-command (concat "open https://github.com/VeracityInnovations/"
                           (projectile-project-name)
                           "/blob/"
                           branch-name
                           file-name
                           "#L"
                           (number-to-string
                            (line-number-at-pos))))))

(global-evil-surround-mode 1)

;; Enable tide/typescript-mode for tsx files
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))