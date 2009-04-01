;;; front-office3.el ---  Major mode for Northgate Front Office V3 Activity XML files

(defvar fov3-mode-hook nil
  "List of functions to call when entering Front Office mode.")

(defvar fov3-mode-map nil
  "Keymap for Front Office Major mode.")

(defun fov3-node-p (l)
  "Is list L an XML node?"
  (and (listp l)
       (atom (car l))
       (listp (car (cdr l)))
       (if (> (length l) 2)
	   (stringp (nth 2 l)))))

(defun fov3-node-eq (name node)
  "Is NODE named NAME?"
  (and (fov3-node-p node)
       (eq (xml-node-name node) name)))

(defun fov3-node-parent-p (node)
  "Is NODE a parent?"
  (and (> (length node) 3)
       (fov3-node-p (nth 3 node))))

(defun fov3-collect-children (node)
  "Get the children of the NODE. Returns a list of children
rather without their whitespace separators."
  (cond
   ((null node) ())
   ((fov3-node-p (car node))
    (cons (car node) (fov3-collect-children (cdr node))))
   (t (fov3-collect-children (cdr node)))))

(defun fov3-newline-p (string)
  "Does STRING start with a newline?"
  (if (stringp string)
      (= (aref string 0) ?\n)
    nil))

;; Need to check the children as well. The name may not match the parent, but it
;; may match one of the children.
(defun fov3-select-nodes (list name)
  "Select all of the nodes with NAME in LIST."
  (cond
   ((null list) ())
   ((fov3-node-p (car list))
    (cond 
     ((eq name (xml-node-name (car list)))
      (cons (car list) (fov3-select-nodes (cdr list) name)))
     ((fov3-node-parent-p (car list))
      (cons (fov3-select-nodes (fov3-collect-children (car list)) name)
	    (fov3-select-nodes (cdr list) name)))
     (t (fov3-select-nodes (cdr list) name))))
   (t (fov3-select-nodes (cdr list) name))))

(if fov3-mode-map
    nil
  (setq fov3-mode-map (make-sparse-keymap))
  (define-key fov3-mode-map "\C-c\p" 'fov3-debug-data))

(defun fov3-parse-buffer ()
  (xml-parse-region (point-min) (point-max)))

(defun fov3--init () 
  (kill-all-local-variables)
  (setq major-mode 'fov3-mode)
  (setq mode-name "Front Office V3")
  (use-local-map fov3-mode-map)
  (run-hooks fov3-mode-hook))

(defun fov3-mode ()
  "Major mode for editing Front Office V3 XML templates.

\\{fov3-mode-map}"
  (interactive)
  (fov3--init)
  (setq service-xml (fov3-parse-buffer)))

(provide 'fov3-mode)

;; (setq visibility 	(fov3-get-nodes 'VisibilityRules))
;; (setq user-code 	(fov3-get-nodes 'UserCode))
;; (setq tables 	(fov3-get-nodes 'FormDefinition))
;; (setq controls	(fov3-get-nodes 'BackOfficeControls))

