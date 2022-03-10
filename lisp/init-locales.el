;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))



;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
(package-refresh-contents)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

(unless (package-installed-p 'key-chord)
  (package-install 'key-chord))

(require 'key-chord)
(key-chord-mode 1)

;; Enable Evil
(require 'evil)
(evil-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)

;; (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
;; (setq evil-undo-system 'undo-tree)


;; sudo apt-get install texlive texlive-xetex texlive-latex-extra
;; sudo apt-get install latex-cjk-all

;; add to org header
;; #+LATEX_HEADER: \usepackage{xeCJK}
;; #+LATEX_HEADER: \setCJKmainfont{SimSun}
;; or
;; #+LATEX_HEADER: \usepackage{ctex}

(require 'org)
;;  org-mode 8.0
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))

;; export cn character
(setf org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

;; (require 'color-theme-sanityinc-tomorrow)

(setq org-capture-templates
      '(
        ("j" "Journal Entry"
         entry (file+datetree "~/Documents/org/journal.org")
         "* %?"
         :empty-lines 1)
        ))

;; (setq org-export-latex-listings t)

(load-theme 'wombat)

(set-face-background 'default "undefined")

(require 'whitespace)
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))
(setq whitespace-style '(face tabs trailing tab-mark))
(set-face-attribute 'whitespace-tab nil
                                        ;foreground "#333333"
                    :foreground "#aaaaaa"
                    ;; :background "#242424"
                    :background "undefined"
                    :weight 'bold)
(set-face-attribute 'whitespace-trailing nil
                    :background "#e4eeff"
                    :foreground "#183bc8"
                    :weight 'normal)
(add-hook 'prog-mode-hook 'whitespace-mode)

                                        ;(setq-default indent-tabs-mode 'only)

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode 'only))))
(add-hook 'find-file-hook 'infer-indentation-style)
(setq-default tab-width 8)
(setq indent-line-function 'insert-tab)

(setq-default c-basic-offset tab-width)
                                        ; (defvaralias 'c-basic-offset 'tab-width)

(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot)

;; (setq-default auto-fill-function 'do-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config)
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode 1)


(require 'origami)
(global-origami-mode 1)

(defun nin-origami-toggle-node ()
  (interactive)
  (if (equal major-mode 'org-mode)
      (org-cycle)
    (save-excursion ;; leave point where it is
      (goto-char (point-at-eol))             ;; then go to the end of line
      (origami-toggle-node (current-buffer) (point)))))                 ;; and try to fold

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local origami-fold-style 'c++-mode)
            (origami-mode)
                                        ;(origami-close-all-nodes (current-buffer))
            ))
(evil-define-key 'normal prog-mode-map (kbd "TAB") 'nin-origami-toggle-node)

(define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
(define-key evil-normal-state-map "zR" 'origami-close-all-nodes)
(define-key evil-normal-state-map "zM" 'origami-open-all-nodes)
(define-key evil-normal-state-map "zr" 'origami-close-node-recursively)
(define-key evil-normal-state-map "zm" 'origami-open-node-recursively)
(define-key evil-normal-state-map "zo" 'origami-show-node)
(define-key evil-normal-state-map "zc" 'origami-close-node)
(define-key evil-normal-state-map "zj" 'origami-forward-fold)
(define-key evil-normal-state-map "zk" 'origami-previous-fold)
(define-key evil-visual-state-map "zf"
  '(lambda ()
     "create fold and add comment to it"
     (interactive)
     (setq start (region-beginning))
     (setq end (region-end))
     (deactivate-mark)
     (and (< end start)
          (setq start (prog1 end (setq end start))))
     (goto-char start)
     (beginning-of-line)
     (indent-according-to-mode)
     (insert comment-start)
     (setq start (point))
     (insert "Folding" " {{{")
     (newline-and-indent)
     (goto-char end)
     (end-of-line)
     (and (not (bolp))
          (eq 0 (forward-line))
          (eobp)
          (insert ?\n))
     (indent-according-to-mode)
     (if (equal comment-end "")
         (insert comment-start " }}}")
       (insert comment-end "}}}"))
     (newline-and-indent)
     (goto-char start)
     ))


(provide 'init-locales)
;;; init-locales.el ends here
