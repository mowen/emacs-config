;; --------------------------------------------------
;; Functions to help with editing martinowen.net

(defun mo-compile-homepage ()
  (interactive)
  (let ((homepage-command-format (if (eq system-type 'darwin)
                                     "cd %s; nanoc co"
                                     "cd %s && nanoc co")))
    (if (boundp 'mo-homepage-dir)
        (compile (format homepage-command-format mo-homepage-dir))
        (message "You need to set the 'mo-homepage-dir' variable."))))

(defun mo-insert-blog-post (title)
  "Blog post skeleton."
  (insert (concat
           "---\n"
           "created_at: " (format-time-string "%Y/%m/%d") "\n"
           "excerpt: A new blog post that needs an excerpt!\n"
           "kind: article\n"
           "publish: true\n"
           "tags: [undefined]\n"
           "title: " title "\n"
           "---\n")))

(defun mo-clean-blog-title (title)
  "Remove everything but letter and numbers, and replace spaces with dash."
  (downcase (replace-regexp-in-string
             "[^a-zA-Z0-9_]" ""
             (replace-regexp-in-string
              " " "_" title))))

(defun mo-new-blog-post (title)
  "Create a new blog post."
  (interactive "sTitle: ")
  (let ((blog-post-dir (concat mo-homepage-dir (format-time-string "/content/blog/%Y")))
        (blog-post-filename (concat (format-time-string "%m-%d-")
                                    (mo-clean-blog-title title) ".org")))
    (if (not (file-exists-p blog-post-dir))
        (dired-create-directory blog-post-dir))
    (find-file (concat blog-post-dir "/" blog-post-filename))
    (mo-insert-blog-post title)))

(global-set-key (kbd "C-c h") 'mo-compile-homepage)
