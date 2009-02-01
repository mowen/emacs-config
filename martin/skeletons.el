(define-skeleton xhtml-trans-skeleton
  "Inserts a skeletal XHTML file with the DOCTYPE declaration for the XHTML 1.0 Transitional DTD"
  nil
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
  "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
  "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
  "<head>\n"
  "<meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" />\n"
  "<meta name=\"Author\" content=\"" (skeleton-read "Enter author: ") "\" />\n"
  "<title>" (skeleton-read "Enter title: ") "</title>\n"
  "</head>\n"
  "<body>\n"
  "\n"
  "</body>\n"
  "</html>"
  '(indent-region (point-min) (point-max))
  '(goto-char (point-max))
  '(forward-line -2)
  '(indent-according-to-mode))

(define-skeleton org-drawer-skeleton
  "Inserts an Org Mode drawer."
  nil
  ":" (upcase (skeleton-read "Drawer name: ")) ":\n\n"
  ":END:"
  '(indent-region (point-min) (point-max))
  '(forward-line -1)
  '(indent-according-to-mode))

(define-skeleton elisp-separator-skeleton
  "Inserts a comment line to separate Emacs Lisp code."
  nil
  ";; -----------------------------------------------------------------------------\n"
  ";; " (skeleton-read "Title: ") "\n")
