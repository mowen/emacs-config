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
  (and (listp node)
       (> (length node) 3)
       (fov3-node-p (nth 3 node))))

(defun fov3-collect-children (node)
  "Get the children of the NODE. Returns a list of children
rather without their whitespace separators."
  (cond
   ((null node) '())
   ((fov3-node-p (car node))
    (cons (car node) (fov3-collect-children (cdr node))))
   (t (fov3-collect-children (cdr node)))))

(defun fov3-newline-p (string)
  "Does STRING start with a newline?"
  (if (stringp string)
      (= (aref string 0) ?\n)
    nil))

;; Why isn't this working?
;; Some of the children don't strip out the strings
(defun fov3--strip-newlines (list)
  "Strip all of the whitespace strings from the service XML node."
  (cond
   ((null list) '())
   ((fov3-node-parent-p (car list))
    (cons (fov3--strip-newlines (fov3-collect-children (car list)))
	  (fov3--strip-newlines (cdr list))))
   ((fov3-node-p (car list))
    (cons (fov3--strip-newlines (car list))
	  (fov3--strip-newlines (cdr list))))
   ((fov3-newline-p (car list)) (fov3--strip-newlines (cdr list)))
   (t (cons (car list) (fov3--strip-newlines (cdr list))))))

;; Need to check the children as well. The name may not match the parent, but it
;; may match one of the children.
(defun fov3-select-nodes (list name)
  "Select all of the nodes with NAME in LIST."
  (cond
   ((null list) '())
   ((fov3-node-p (car list))
    (cond 
     ((eq name (xml-node-name (car list)))
      (cons (car list) (fov3-select-nodes (cdr list) name)))
     ((fov3-node-parent-p (car list))
      (cons (fov3-select-nodes (fov3-collect-children (car list)) name)
	    (fov3-select-nodes (cdr list) name)))
     (t (fov3-select-nodes (cdr list) name))))
   (t (fov3-select-nodes (cdr list) name))))

(defun fov3-debug-data ()
  "Display fov3-service-xml in a temporary buffer."
  (interactive)
  (let ((new-buffer (generate-new-buffer "fov3-node")))
    (set-buffer new-buffer)
    (insert (format "%S" fov3-service-xml))
    (switch-to-buffer new-buffer))
    (goto-char (point-min)))

(defun fov3--load-service ()
  "Load all of the service data.")

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
  (setq fov3-service-xml (car (fov3-parse-buffer))))

(provide 'fov3-mode)

;; (setq visibility 	(fov3-get-nodes 'VisibilityRules))
;; (setq user-code 	(fov3-get-nodes 'UserCode))
;; (setq tables 	(fov3-get-nodes 'FormDefinition))
;; (setq controls	(fov3-get-nodes 'BackOfficeControls))

(require 'tree-widget)
(require 'tree-mode)

(defun xml-attributes-widget (attribute)
  "Generate an editable field for attribute."
  (cond
   ((null attributes) nil)
   (t '((text :format "%s: " :value (car attribute))
	(editable-field :value (cdr attribute)
			:notify (lambda (widget &rest ignore)
				  (message (widget-value widget))))))))

(defun xml-tree-widget (root)
  "Generate a tree widget representing the XML in ROOT."
  (cond
   ((null root) nil)
   ((listp root)
    (let ((elem (xml-node-name root))
	  (attributes (xml-node-attributes root))
	  (children (remove-if (function stringp) (xml-node-children root))))
      `(tree-widget :node (push-button
			   :tag ,(format "%s" elem)
			   :format "%[%t%]\n"
			   :xml-node ,root
			   :notify ,(lambda (widget &rest rest)
				      (message (format "%s"
						       (widget-get widget :xml-node)))))
		    ,@(mapcar (lambda (x) (xml-attributes-widget x)) attributes)
		    ,@(mapcar (lambda (x) (xml-tree-widget x)) children))))))

(defun fov3-create-tree-widget ()
  (interactive)
  (let ((new-buffer (generate-new-buffer "fov3-xml-tree")))
    (set-buffer new-buffer)
    (widget-apply-action
     (widget-create (xml-tree-widget (car service-xml))))
    (switch-to-buffer new-buffer)
    (tree-mode)
    (goto-char (point-min))))

(setq service-xml (xml-parse-file "MOpestCont.xml"))