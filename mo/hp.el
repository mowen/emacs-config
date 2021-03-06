;; --------------------------------------------------
;; Functions to help with editing martinowen.net

(defvar mo-homepage-server-process-name "homepage server"
  "The name of my homepage server process.")

(defvar mo-homepage-dev-url "http://martinowen.net.dev:3000"
  "The dev URL of my homepage.")

(defun mo-compile-homepage ()
  "Run nanoc to compile my homepage."
  (interactive)
  (let ((homepage-command-format (if (eq system-type 'darwin)
                                     "cd %s; nanoc co"
                                     "cd %s && nanoc co")))
    (if (boundp 'mo-homepage-dir)
        (compile (format homepage-command-format mo-homepage-dir))
        (message "You need to set the 'mo-homepage-dir' variable."))))

(defun mo-start-homepage-server ()
  "Start the thin server process for my homepage. Only actually
necessary on Windows as it runs under Pow on OS X."
  (interactive)
  (cd mo-homepage-dir) ;; improve this so that it doesn't modify default-directory
  (start-process mo-homepage-server-process-name mo-homepage-server-process-name "thin" "start"))

(defun mo-browse-dev-homepage ()
  (interactive)
  (browse-url mo-homepage-dev-url))

(defun mo-stop-homepage-server ()
  "Stop the thin server process for my homepage."
  (interactive)
  (delete-process mo-homepage-server-process-name))

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

(defun mo-list-blog-posts (blog-dir)
  "List all blog post filenames."
  (let ((order 0)
        (result '()))
    (mapc (lambda (dir)
            (mapc (lambda (file)
                    (setq order (1+ order))
                    (setq file (substring file (length (expand-file-name blog-dir))))
                    (setq result (cons (list file order) result)))
                  (directory-files dir t "[^.]")))
          (directory-files blog-dir t "[^.]"))
    result))

(defun mo-find-blog-post ()
  "Use completion to find blog post."
  (interactive)
  (let* ((blog-dir (concat mo-homepage-dir "/content/blog"))
         (blog-post-filenames (mo-list-blog-posts blog-dir))
         (blog-post-filename (completing-read "Blog Post: " blog-post-filenames nil t nil nil)))
    (find-file (concat blog-dir blog-post-filename))))

(global-set-key (kbd "C-c h") 'mo-compile-homepage)
                
