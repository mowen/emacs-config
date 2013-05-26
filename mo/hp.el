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

(define-skeleton mo-blog-post "Blog post skeleton."
  "Title: "
  "---" \n
  "created_at: " (format-time-string "%Y/%m/%d") \n
  "excerpt: A new blog post that needs an excerpt!" \n
  "kind: article" \n
  "publish: true" \n
  "tags: [undefined]" \n
  "title: " str | "New Blog Post!" \n
  "---" \n
  _)

;; TODO: format the filename of the new post using the title
(defun mo-new-blog-post ()
  "Create a new blog post."
  (interactive)
  (let ((blog-post-dir (concat mo-homepage-dir (format-time-string "/content/blog/%Y"))))
    (if (not (file-exists-p blog-post-dir))
        (dired-create-directory blog-post-dir))
    (find-file (concat blog-post-dir "/new_post.org"))
    (call-interactively 'mo-blog-post)))

(global-set-key (kbd "C-c h") 'mo-compile-homepage)
