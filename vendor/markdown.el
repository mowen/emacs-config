;;; markdown.el --- Provides some extra commands to make it easier to edit markdown files.

;; Copyright (C) 2007 James Wright

;; Author: James Wright <james@chumsley.org>
;; Created: 12 May 2007

;; This file is not yet part of GNU Emacs.

;; markdown.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; markdown.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Requires filladapt.el, which is available from <http://www.wonderworks.com/download/filladapt.el>
;;
;; This is a mode to highlight markdown code.  It deviates from John
;; Gruber's markdown standard in that asterisks automatically mean
;; bold and underscores mean italics.  There is a
;; `markdown-make-heading-command' that cycles between heading levels.

;;; Code:

(require 'filladapt)

;;;; ======================================= major-mode definition ======================================

(defvar markdown-mode-map
  (let ((map (make-sparse-keymap 'markdown-mode-map)))
    (set-keymap-parent map text-mode-map)
    (define-key map [?\r] 'markdown-newline-and-indent/maybe-bullet)
    (define-key map (kbd "C-=") 'markdown-make-heading-command)
    (define-key map [(control ?\ )] 'markdown-toggle-checkmark)
    (define-key map [?\] ] 'markdown-electric-bracket)
    map)
  "Keymap for markdown-mode")

(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for editing text documents with markdown formatting"
  (if running-xemacs
    (put 'markdown-mode 'font-lock-defaults '(markdown-font-lock-keywords nil t nil nil))
    (setq font-lock-defaults '(markdown-font-lock-keywords)))
  (use-local-map markdown-mode-map))

;;;; =============================================== faces ==============================================

(defface markdown-heading-1-face
    '((((class color) (background dark))
       (:bold t :foreground "red" :background "white"))
      (((class color) (background light))
       (:bold t :foreground "red" :background "white"))
      (t (:bold t :inverse-video t)))
  "Face used for level 1 markdown headings"
  :group 'markdown)
(setq markdown-heading-1-face 'markdown-heading-1-face)

(defface markdown-heading-2-face
    '((((class color) (background dark))
       (:bold t :foreground "black" :background "white"))
      (((class color) (background light))
       (:bold t :foreground "black" :background "white"))
      (t (:bold t :inverse-video t)))
  "Face used for level 2 markdown headings"
  :group 'markdown)
(setq markdown-heading-2-face 'markdown-heading-2-face)

(defface markdown-heading-3-face
    '((((class color) (background dark))
       (:bold t :foreground "gray" :background "white"))
      (((class color) (background light))
       (:bold t :foreground "gray" :background "white"))
      (t (:bold t :inverse-video t)))
  "Face used for level 3 markdown headings"
  :group 'markdown)
(setq markdown-heading-3-face 'markdown-heading-3-face)

(defface markdown-blockquote-face
    '((((class color) (background dark))
       (:foreground "darkmagenta"))
      (((class color) (background light))
       (:foreground "darkmagenta"))
      (t (:italic t)))
  "Face used for quoted text"
  :group 'markdown)
(setq markdown-blockquote-face 'markdown-blockquote-face)

(defface markdown-emphasized-face
    '((((class color) (background dark))
       (:italic t))
      (((class color) (background light))
       (:italic t))
      (t (:italic t)))
  "Face used for emphasized text"
  :group 'markdown)
(setq markdown-emphasized-face 'markdown-emphasized-face)

(defface markdown-strong-face
    '((((class color) (background dark))
       (:bold t))
      (((class color) (background light))
       (:bold t))
      (t (:bold t)))
  "Face used for strong text"
  :group 'markdown)
(setq markdown-strong-face 'markdown-strong-face)

;; FIXME Doesn't currently work in emacs
;(defface markdown-strong-emphasized-face
;    '((((class color) (background dark))
;       (:bold t) (:italic t))
;      (((class color) (background light))
;       (:bold t) (:italic t))
;      (t (:bold t) (:italic t)))
;  "Face used for strong and emphasized text"
;  :group 'markdown)

(defface markdown-strong-emphasized-face
    '((((class color) (background dark))
       (:bold t))
      (((class color) (background light))
       (:bold t))
      (t (:bold t)))
  "Face used for strong and emphasized text"
  :group 'markdown)
(setq markdown-strong-emphasized-face 'markdown-strong-emphasized-face)

(defface markdown-code-face
    '((((class color) (background dark))
       (:foreground "darkred"))
      (((class color) (background light))
       (:foreground "darkred"))
      (t (:foreground "darkred")))
  "Face used for teletype text (usually source-code)"
  :group 'markdown)
(setq markdown-code-face 'markdown-code-face)

(defface markdown-reference-face
    '((((class color) (background dark))
       (:foreground "blue4"))
      (((class color) (background light))
       (:foreground "blue4"))
      (t (:foreground "blue4")))
  "Face used for emacs-style references"
  :group 'markdown)
(setq markdown-reference-face 'markdown-reference-face)

(defface markdown-hyperlink-face
    '((((class color) (background dark))
       (:foreground "blue4"))
      (((class color) (background light))
       (:foreground "blue4"))
      (t (:foreground "blue4")))
  "Face used for hyperlinks"
  :group 'markdown)
(setq markdown-hyperlink-face 'markdown-hyperlink-face)

(defface markdown-citation-face
    '((((class color) (background dark))
       (:foreground "blue4"))
      (((class color) (background light))
       (:foreground "blue4"))
      (t (:foreground "blue4")))
  "Face used for citations"
  :group 'markdown)
(setq markdown-citation-face 'markdown-citation-face)

(defface markdown-math-face
    '((t :inherit font-lock-string-face))
  "Face used for LaTeX-style inline math"
  :group 'markdown)
(setq markdown-math-face 'markdown-math-face)

(defvar markdown-font-lock-keywords
  '(
    ;; Priority tags
    ("\\(?: \\|^\\)\\(\\*\\*+\\|!!+\\|\\?\\?+\\)\\(?: \\|$\\)" 1 font-lock-warning-face)
    
    ;; Moin-Moin style headings
    ("^===+ \\(.*\\)\\([ \t]+=*\\)?[ \t]*$" 0 markdown-heading-3-face)
    ("^== \\(.*\\)\\([ \t]+=*\\)?[ \t]*$"   0 markdown-heading-2-face)
    ("^= \\(.*\\)\\([ \t]+=*\\)?[ \t]*$"    0 markdown-heading-1-face)
    
    ;; setext style headings
    ;; (We're using "anchor matchers" here.  Basically, iff we encounter a line that looks like it
    ;; might be an underline, the PRE-MATCH extends the beginning of the match check to the start of
    ;; the next line and optionally highlights it too.  So hitting enter after the underline no
    ;; longer causes its highlight to disapper, although editing the heading text will cause it to
    ;; disapper)
    ("^==+[ \t]*$" (0 markdown-heading-1-face)
     ("^[ \t]*[^ \t].*$"
      (let ((e (point-at-eol))) (forward-line -1) (goto-char (point-at-bol)) e)
      nil
      (0 markdown-heading-1-face)))
    ("^--+[ \t]*$" (0 markdown-heading-2-face)
     ("^[ \t]*[^ \t].*$"
      (let ((e (point-at-eol))) (forward-line -1) (goto-char (point-at-bol)) e)
      nil
      (0 markdown-heading-2-face)))
    ("^~~+[ \t]*$" (0 markdown-heading-3-face)
     ("^[ \t]*[^ \t].*$"
      (let ((e (point-at-eol))) (forward-line -1) (goto-char (point-at-bol)) e)
      nil
      (0 markdown-heading-3-face)))

    ;; atx style headings
    ("^###+.*$" 0 markdown-heading-3-face)
    ("^##.*$" 0   markdown-heading-2-face)
    ("^#.*$" 0    markdown-heading-1-face)

    ;; Block-quotes
    ("^> .*$" 0 markdown-blockquote-face)

    ;; Strong and emphasized text
    ("\\(^\\|[ \t'\"(<[]\\)\\(__[^_ \t]__\\)\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-strong-face append)
    ("\\(^\\|[ \t'\"(<[]\\)\\(__[^_ \t].*?[^ \t]__\\)\\($\\|[ \t'\",.:;!?)]\\)"
     2 markdown-strong-face append)

    ("\\(^\\|[ \t'\"(<[]\\)_\\([^_ \t]\\)_\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-emphasized-face append)
    ("\\(^\\|[ \t'\"(<[]\\)_\\([^_ \t].*?[^ \t]\\)_\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-emphasized-face append)

    ("\\(^\\|[ \t'\"(<[]\\)/\\([^/ \t]\\)/\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-emphasized-face append)
    ("\\(^\\|[ \t'\"(<[]\\)/\\([^/ \t].*?[^ \t]\\)/\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-emphasized-face append)
    
    ("\\(^\\|[ \t'\"(<[]\\)\\(\\*\\*[^* \t]\\*\\*\\)\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-strong-emphasized-face append)
    ("\\(^\\|[ \t'\"(<[]\\)\\(\\*\\*[^* \t].*?[^ \t]\\*\\*\\)\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-strong-emphasized-face append)
    
    ("\\(^\\|[ \t'\"(<[]\\)\\(\\*[^* \t]\\*\\)\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-strong-face append)
    ("\\(^\\|[ \t'\"(<[]\\)\\(\\*[^* \t].*?[^ \t]\\*\\)\\($\\|[] \t'\",.:;!?>)]\\)"
     2 markdown-strong-face append)

    ;; Source code and references
    ("\\(^\\|[ \t'\"(<]\\)`\\([^ \t]\\)'\\($\\|[] \t'\",.:;!?>)]\\)" 2 markdown-reference-face)
    ("\\(^\\|[ \t'\"(<]\\)`\\([^ \t][^`]*?[^ \t]\\)'\\($\\|[] \t'\",.:;!?>)]\\)" 2 markdown-reference-face)

    ("\\(^\\|[ \t'\"(<]\\)`\\([^ \t]\\)`\\($\\|[] \t'\",.:;!?>)]\\)" 2 markdown-code-face)
    ("\\(^\\|[ \t'\"(<]\\)`\\([^ \t][^`]*?[^ \t]\\)`\\($\\|[] \t'\",.:;!?>)]\\)" 2 markdown-code-face)

    ("\\(^\\|[ \t'\"(<]\\)<\\([^ \t@:]\\)>\\($\\|[] \t'\",.:;!?>)]\\)" 2 markdown-emphasized-face t)
    ("\\(^\\|[ \t'\"(<]\\)<\\([^ \t@:][^@:]*?[^ \t@:]\\)>\\($\\|[] \t'\",.:;!?>)]\\)" 2 markdown-emphasized-face t)

    ("\\(^\\|[ \t'\"(<]\\)<\\([^ \t]\\)>\\($\\|[] \t'\",.:;!?>)]\\)" 2 markdown-hyperlink-face)
    ("\\(^\\|[ \t'\"(<]\\)<\\([^ \t].*?[^ \t]\\)>\\($\\|[] \t'\",.:;!?>)]\\)" 2 markdown-hyperlink-face)

    ;; Citations
    ("\\[[^]]*[0-9][0-9]\\([0-9][0-9]\\)?[a-z]*\\]" 0 markdown-citation-face)

    ;; LaTeX-style math
    ("\\(^\\|[ \r\n-]\\)\\(\\$[^$]*[^$ \r\n]\\$\\)\\($\\|[^a-zA-Z0-9]\\)" 2 markdown-math-face)
    )
  "Font-lock highlighting regular expressions for editing markdown buffers")


;;;; ======================================= function definitions =======================================

(defun markdown-strip-heading ()
  "Strip out heading underlines/same-lines and leave just the regular text on the current line."

  ;; Delete MoinMoin heading indicators if any
  (save-excursion
    (goto-char (point-at-bol))
    (when (looking-at "[ \t]*=+ \\(.*\\) =+$")
      (let ((text (match-string 1)))
        (delete-region (point-at-bol) (point-at-eol))
        (insert text))))
                  
  ;; Delete underline if any
  (save-excursion
    (forward-line 1)
    (when (looking-at "[ \t]*[-=~][-=~]+[ \t]*$")
      (delete-region (point-at-bol) (point-at-eol))
      (delete-char -1))))
  
(defun markdown-make-single-line-heading (n)
  "Makes the current line a level-N heading with no underline"
  (let ((hdr ""))
    (while (> n 0)
      (setq hdr (concat hdr "="))
      (setq n (- n 1)))
    (markdown-strip-heading)
    (goto-char (point-at-bol))
    (insert hdr " ")
    
    (goto-char (point-at-eol))
    (insert " " hdr)))

(defun markdown-make-underline-heading (str)
  "Makes the current line a heading that is underlined by STR (and optionally 'toplined' by TOPLINE)"
  (markdown-strip-heading)
  (let ((len (length (buffer-substring (point-at-bol) (point-at-eol))))
        (hdr ""))
    (while (> len 0)
      (setq hdr (concat hdr str))
      (setq len (- len (length str))))
    (forward-line 1)
    (insert hdr)
    ;; HACK - update the text on the previous line to get font-lock to update its font as well.
    (save-excursion
      (forward-line -1)
      (goto-char (point-at-bol))
      (insert " ")
      (delete-region (point) (point-at-bol)))))
    
    
;;TODO topline heading

(defun markdown-heading-level ()
  "Returns the heading level of the current line, or NIL for non-headings."
  (let ((line-text (buffer-substring (point-at-bol) (point-at-eol)))
        (prev-line-text (save-excursion
                          (when (zerop (forward-line -1))
                            (buffer-substring (point-at-bol) (point-at-eol)))))
        (next-line-text (save-excursion
                          (when (zerop (forward-line 1))
                            (buffer-substring (point-at-bol) (point-at-eol))))))
    (cond
      ((and prev-line-text
            (string-match "^[-=~][-=~]+$" line-text))
       (save-excursion
         (forward-line -1)
         (markdown-heading-level)))
      ((string-match "^\\(=+\\) .* =+$" line-text)
       (length (match-string 1 line-text)))
      ((and next-line-text
            (string-match "^==+$" next-line-text))
       1)
      ((and next-line-text
            (string-match "^--+$" next-line-text))
       2)
      ((and next-line-text
            (string-match "^~~+$" next-line-text))
       3))))

(defun markdown-make-heading-command ()
  "Makes the current line a markdown heading.  If it is already a
markdown heading, toggles between the three heading styles"
  (interactive "*")
  (let* ((line-text (buffer-substring (point-at-bol) (point-at-eol)))
         (prev-line-text (save-excursion
                           (when (zerop (forward-line -1))
                             (buffer-substring (point-at-bol) (point-at-eol)))))
         (next-line-text (save-excursion
                           (when (zerop (forward-line 1))
                             (buffer-substring (point-at-bol) (point-at-eol)))))
         (heading-level (markdown-heading-level))
         (underline-char (nth (or heading-level 0)
                              '(nil "=" "-" "~"))) ;TODO use customizable vars
         (underline-p (nth (or heading-level 0)
                           '(t t t nil)))) ;TODO use customizable vars
    (cond
      ;; If the cursor is on the underline portion of a heading, then back up to the text and try
      ;; again
      ((and heading-level
            prev-line-text
            (string-match (format "^%s%s+$" underline-char underline-char) line-text))
       (forward-line -1)
       (markdown-make-heading-command))

      ;; Otherwise, if we want an underline and an existing one is the wrong length, adjust it
      ((and heading-level
            underline-p
            (string-match (format "^%s%s+$" underline-char underline-char) next-line-text)
            (/= (length line-text) (length next-line-text)))
       (markdown-make-heading heading-level))

      ;; Otherwise, if this is a heading, then cycle to the next level
      (heading-level
       (markdown-make-heading (1+ (mod heading-level 3))))

      ;; Otherwise, make this line a heading.  Choose the level to use by searching back for the
      ;; most recent heading.
      (t
       (let ((old-heading nil))
         (save-excursion
           (while (and (null old-heading)
                       (zerop (forward-line -1)))
             (setq old-heading (markdown-heading-level))))
         (markdown-make-heading (or old-heading 1)))))))

(defun markdown-make-heading (level)
  (markdown-strip-heading)
  (let ((underline-char (nth (or level 0)
                             '(nil "=" "-" "~"))) ;TODO use customizable vars
        (underline-p (nth (or level 0)
                          '(t t t nil))) ;TODO use customizable vars
        (len (length (buffer-substring (point-at-bol) (point-at-eol))))
        (hdr ""))

    (cond
      (underline-p
       (goto-char (point-at-eol))
       (insert "\n")
       (while (> len 0)
         (insert underline-char)
         (setq len (- len 1))))
      (t
       (while (> level 0)
         (setq hdr (concat hdr "="))
         (setq level (- level 1)))
       (goto-char (point-at-bol))
       (insert hdr " ")
       (goto-char (point-at-eol))
       (insert " " hdr)))))
      
(defun markdown-toggle-checkmark ()
  "If the current paragraph is a checkbox item, toggle the checkbox on or off"
  (interactive)
  (save-excursion
    (markdown-beginning-of-paragraph)
    (when (looking-at "[ \t]*\\[\\([ X]\\)\\][ \t]")
      (goto-char (match-beginning 1))
      (cond
        ((string= "X" (match-string 1))
         (delete-char 1)
         (insert " "))
        ((string= " " (match-string 1))
         (delete-char 1)
         (insert "X"))))))

(defun markdown-electric-bracket ()
  (interactive)
  (insert "]")
  (let ((post-insert ""))
    (save-excursion
      (backward-char 2)
      (when (looking-at "\\[\\]")
        (forward-char)
        (insert " ")
        (forward-char)
        (setq post-insert " ")))
    (insert post-insert)))
(put 'markdown-electric-bracket 'delete-selection t)

(defun markdown-word-bounds (&optional re len)
  "Returns a cell of the form (S . E), where S is the beginning of the current word and E is the end.
If the region is active, returns the bounds of the region instead.  The bounds will extend beyond
the current word/region if the word/region is surrounded by text that matches RE."
  (let* ((s (cond
              ((region-active-p)
               (region-beginning))
              ((looking-at "\\<")
               (point))
              (t
               (save-excursion
                (backward-word)
                (point)))))
         (e (if (region-active-p)
              (region-end)
              (save-excursion
                (goto-char s)
                (forward-word)
                (point)))))
    (if (null re)
      (cons s e)
      (save-excursion
        (goto-char s)
        (backward-char)
        (while (looking-at re)
          (setq s (point))
          (backward-char))
        (goto-char e)
        (while (looking-at re)
          (forward-char)
          (setq e (point)))
        (cons s e)))))

(defun markdown-toggle-emphasized ()
  "If the region is active, makes the current region emphasized; otherwise makes the current word
emphasized.  If the current region/word is already emphasized, makes it strong; if it's already
strong, removes all emphasis."
  (interactive "*_")
  (let* ((bounds (markdown-word-bounds "_" 1))
         (s (car bounds))
         (e (cdr bounds))
         (word (buffer-substring s e)))
    (push-mark s)
    (goto-char e)
    (delete-region s e)
    (if (string-match "__\\(.*\\)__" word)
      (insert (match-string 1 word))
      (insert "_" word "_"))
    (when (region-active-p)
      (zmacs-update-region)
      (putf this-command-properties 'shifted-motion-command t))))

(defun markdown-para-level/type ()
  "Returns the indentation level and type of the current paragraph"
  (save-excursion
    (markdown-beginning-of-paragraph)
    (cond
      ((looking-at "^\\([ \t]*\\)[-+*][ \t]")
       (cons (length (match-string 1)) 'bullet))
      ((looking-at "^\\([ \t]*\\)\\[ \\][ \t]")
       (cons (length (match-string 1)) 'bullet))
      ((looking-at "^\\([ \t]*\\)[0-9]+\\.[ \t]")
       (cons (length (match-string 1)) 'itemized)))))

(defun markdown-para-level ()
  "Returns the type of the current paragraph"
  (car (markdown-para-level/type)))

(defun markdown-para-type ()
  "Returns the type of the current paragraph"
  (cdr (markdown-para-level/type)))

(defun markdown-beginning-of-paragraph ()
  "Step to the beginning of the current paragraph"
  (save-restriction
    (let ((fill-prefix fill-prefix))
      (when (filladapt-adapt t t)
        (goto-char (point-min))))))

(defun markdown-beginning-of-previous-paragraph ()
  "Step to the beginning of the previous paragraph"
  (let ((s (point)))
    (markdown-beginning-of-paragraph)
    (forward-line -1)
    (markdown-beginning-of-paragraph)
    (< (point) s)))

(defun markdown-find-predecessor (level type)
  "Returns the first line of the first paragraph before the current one that has
the specified LEVEL and TYPE."
  (save-excursion
    (let ((verdict 'keep-looking))
      (while (and (eq verdict 'keep-looking)
                  (markdown-beginning-of-previous-paragraph))
        (let ((level/type (markdown-para-level/type)))
          (setq verdict
                (cond
                  ((looking-at "^[ \t]*$")
                   'keep-looking)
                  ((null level/type)
                   nil)
                  ((< (car level/type) level)
                   nil)
                  ((> (car level/type) level)
                   'keep-looking)
                  ((eq (cdr level/type) type)
                   (buffer-substring (point-at-bol) (point-at-eol)))))))
      verdict)))

(defun markdown-next-para-prefix ()
  (let ((fill-prefix nil)
        (src-column (current-column)))
    (save-excursion
      (markdown-beginning-of-paragraph)
      (cond
        ;; no prefix if the current para-start is a horizontal rule
        ((looking-at "\\([ \t]*[*-]\\)\\([ \t]*[*-]\\)\\([ \t]*[*-]\\)+[ \t]*$") nil)

        ;; no prefix if we hit enter with the cursor preceding a bullet or the number on an
        ;; ordered list
        ((and (looking-at "[ \t]*\\([*+-]\\|[0-9]+\\.\\|\\[ \\]\\)[ \t]+")
              (< src-column (length (match-string 0))))
         nil)

        ;; Delete existing bullet if we hit enter on an empty bullet (or ordered list) and return
        ;; no prefix (so we'll newline and indent).  This is a bit of hack, but it works well
        ;; enough.
        ;; TODO It might be nice if we could "out-dent" for nested items.
        ((looking-at "[ \t]*\\([*+-]\\|[0-9]+\\.\\|\\[ \\]\\)[ \t]+$")
         (delete-region (point-at-bol) (point-at-eol))
         (delete-backward-char 1)
         nil)

        ;; Prefix after a bullet is just another bullet at the same level, possibly preceded by a
        ;; newline.
        ((looking-at "[ \t]*\\([*+-]\\|\\[ \\]\\)[ \t]+")
         (let ((prefix (match-string 0))
               (level/type (markdown-para-level/type)))
           (if (and (zerop (forward-line -1))
                    (looking-at "^[ \t]*$")
                    (markdown-find-predecessor (car level/type) (cdr level/type)))
             (concat "\n" prefix)
             prefix)))

        ;; prefix after an ordered list is the next number in the list, plus the same amount of
        ;; following whitespace, with the proviso that if the next number has more digits, we'll
        ;; return less following whitespace if possible.  Maybe we want a leading newline, maybe
        ;; not.
        ((looking-at "\\([ \t]*\\)\\([0-9]+\\)\\(\\.[ \t]\\)\\([ \t]*\\)")
         (let* ((pre (match-string 1))
                (num (match-string 2))
                (sep (match-string 3))
                (post (match-string 4))
                (next-num (format "%d" (+ 1 (car (read-from-string num)))))
                (delta (- (length next-num) (length num)))
                (prefix (concat pre next-num sep 
                                (if (< delta (length post))
                                  (substring post delta)
                                  "")))
                (level/type (markdown-para-level/type)))
           (if (and (zerop (forward-line -1))
                    (looking-at "^[ \t]*$")
                    (markdown-find-predecessor (car level/type) (cdr level/type)))
             (concat "\n" prefix)
             prefix)))))))

(defun markdown-newline-and-indent/maybe-bullet ()
  "Insert a newline and indent to the same column as the original line.  If the starting line is in
  a bulleted paragraph, then also add a new bullet."
  (interactive)
  (let ((target-column (min (current-column)
                            (current-indentation)))
        (next-prefix (markdown-next-para-prefix)))
    (newline)
    (if next-prefix
      (insert next-prefix)
      (indent-to target-column))))

(provide 'markdown)
;;; markdown.el ends here