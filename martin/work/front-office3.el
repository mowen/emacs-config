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
	   (or (stringp (nth 2 l))
	       (listp (nth 2 l)))
	 t)))

(defun fov3-node-eq (name node)
  "Is NODE named NAME?"
  (and (fov3-node-p node)
       (eq (xml-node-name node) name)))

(defun fov3-node-parent-p (node)
  "Is NODE a parent?"
  (and (listp node)
       (> (length node) 2)
       (fov3-node-p (nth 2 node))))

(defun fov3-get-children (node)
  "Get the children of the NODE."
  (nthcdr 2 node))

(defun fov3-newline-p (string)
  "Does STRING start with a newline?"
  (if (and (stringp string)
	   (> (length string) 0))
      (= (aref string 0) ?\n)
    nil))

;; TODO: Recursion may not be the best option here
(defun fov3-remove-newlines (root)
  "Strip all of the whitespace strings from ROOT."
  (cond 
   ((null root) '())
   ((fov3-newline-p root) '())
   ((stringp root) root)
   ((fov3-newline-p (car root))
    (fov3-remove-newlines (cdr root)))
   ((or (stringp (car root))
	(atom (car root)))
    (cons (car root) (fov3-remove-newlines (cdr root))))
   ((listp (car root))
    (cons (fov3-remove-newlines (car root))
	  (fov3-remove-newlines (cdr root))))))

(defun fov3-normalize-root (root)
  "Prepare the root for displaying via fov3-debug-data."
  (if (not (fov3-node-p root))
      (if (= 1 (length root))
	  ;; First element must be a node, so use that
	  (car root)
	;; if length != 1 and not a node then we must provide a root element
	`(Fov3DebugRoot nil ,@root))
    root))

(defun fov3-restore-newlines (root)
  "Restore newlines according to xml.el indentation."
  (setq root (fov3-normalize-root root))
  (with-temp-buffer
    (xml-debug-print (cons root '()))
    (xml-parse-region (point-min)
		      (point-max)
		      (current-buffer))))

;; -----------------------------------------------------------------------------
;; Node selection

;; TODO: Allow the option of an attribute to select on too.
;; Attribute will be given as a single element assoc list.
(defun fov3-select-nodes (root name)
  "Select all of the nodes with NAME in ROOT."
  (cond
   ((null root) '())
   ((fov3-node-p (car root))
    (cond 
     ((eq name (xml-node-name (car root)))
      (cons (car root) (fov3-select-nodes (cdr root) name)))
     ((fov3-node-parent-p (car root))
      ;; Using append rather than cons as it will remove the nils returned by
      ;; (fov3-select-nodes (cdr root) name). Will cause problems if I want to
      ;; preserve nested structure, but that should only happen if a node name
      ;; occurs at more than one level (Style is an example.)
      (append (fov3-select-nodes (fov3-get-children (car root)) name)
	      (fov3-select-nodes (cdr root) name)))
     (t (fov3-select-nodes (cdr root) name))))
   (t (fov3-select-nodes (cdr root) name))))

(defun fov3-node-has-attr-value (node attr-name attr-value)
  "Check if the NODE's attribute with ATTR-NAME has ATTR-VALUE."
  (let ((attr (fov3-node-get-attribute node attr-name)))
    (if (null attr)
	nil
      (string= attr-value (cdr attr)))))

(defun fov3-node-id (node)
  "Get the 'id' attribute of NODE."
  (fov3-node-get-attribute node 'id))

;; TODO: Make the parameters to this consistent with the node-get/set-attribute
;; functions (or should this be able to take more than one attr?)
(defun fov3-select-node-with-attr (root name attr-alist)
  "Select the node with NAME and ATTRibute."
  (let ((node-list (fov3-select-nodes root name)))
    (setq node-list
	  (mapcar '(lambda (node)
		     (if (fov3-node-has-attr-value node
						   (car attr-alist)
						   (cdr attr-alist))
			 node)) node-list))
    (car (remove-if (function null) node-list))))

;; NOTE copy-tree can be used to copy a nested list

;; TODO: The references shouldn't need to include the node name and attributes
;; (basically just add 2 to each.)
(defun fov3-get-tree (root list-of-refs)
  "Select the (potentially nested) tree in ROOT given by
LIST-OF-REFS."
  (cond
   ((null list-of-refs) '())
   ((null (cdr list-of-refs)) (nth (car list-of-refs) root))
   (t (fov3-get-tree (nth (car list-of-refs) root) (cdr list-of-refs)))))

(defun fov3-node-get-attribute (node attr-name)
  "Get the NODE's attribute with ATTR-NAME."
  (let ((node-attrs (xml-node-attributes node)))
    (assoc attr-name node-attrs)))

(defun fov3-node-set-attribute (node attr-name attr-value)
  "Set the NODE's attribute."
  (let ((attr (fov3-node-get-attribute node attr-name)))
    (setcdr attr attr-value)
    (set-buffer-modified-p t)))
  
;; -----------------------------------------------------------------------------
;; User Code

(defun fov3-user-code-count ()
  "Get the number of user code entries."
  (length (fov3-select-nodes fov3-user-code 'Code)))

(defun fov3-get-user-code (id)
  "Get the user code with ID."
  (fov3-select-node-with-attr fov3-user-code 'Code `(id . ,id)))

(defun fov3-get-user-code-body (user-code)
  "Get the user code body from USER-CODE."
  (nth 2 (car (fov3-select-nodes user-code 'CodeBody))))

(defun fov3-set-user-code-body (body-node new-body)
  "Set the user code BODY-NODE to have body string NEW-BODY."
  (let ((old-body (fov3-get-children body-node)))
    (setcar old-body new-body)
    (set-buffer-modified-p t)))

;; TODO Change the local variable that I need in the save function
;; to buffer local variables
(defun fov3-edit-user-code-body (id)
  "Load the user code body with ID into a buffer."
  (interactive "p")
  ;; TODO check how many user code elements there are
  (let* ((id (number-to-string (or id 1)))
	 (user-code (fov3-get-user-code id))
	 (user-code-type (cdr (assoc 'codetype (xml-node-attributes user-code))))
	 (user-code-body (fov3-get-user-code-body user-code))
	 (old-buffer-name (buffer-name (current-buffer)))
	 (new-buffer-name (concat "fov3-user-code-" id))
	 (new-buffer (generate-new-buffer new-buffer-name)))
    (set-buffer new-buffer)
    (insert user-code-body)
    (switch-to-buffer new-buffer)
    (if (string= user-code-type "J")
	(js2-mode)
      (csharp-mode))
    (local-set-key (kbd "C-x C-s")
		   `(lambda ()
		      (interactive)
		      (let ((new-code-body (buffer-substring (point-min) (point-max))))
			(set-buffer ,old-buffer-name)
			(fov3-set-user-code-body
			 (car (fov3-select-nodes (quote ,user-code) 'CodeBody))
			 new-code-body)
			(set-buffer ,new-buffer-name)
			(message "Saved."))))
    (goto-char (point-min))))

;; -----------------------------------------------------------------------------
;; Form Definition Manipulation

(defsubst fov3-table-count ()
  "Get the number of tables in the service."
  (length (fov3-select-nodes fov3-form-definition 'Table)))

(defsubst fov3-row-count (table)
  "Get the number of rows in TABLE."
  (length (fov3-select-nodes table 'Row)))

(defsubst fov3-column-count (row)
  "Get the number of columns in ROW."
  (length (fov3-select-nodes row 'Column)))

;; -----------------------------------------------------------------------------
;; Debugging
  
(defun fov3-debug-data (lst &optional buffer-name)
  "Display Front Office V3 node LIST in a temporary buffer."
  (interactive)
  (let* ((new-buffer-name (or buffer-name "fov3-node"))
	 (new-buffer (generate-new-buffer new-buffer-name))
	 (lst-with-newlines (fov3-restore-newlines lst)))
    (set-buffer new-buffer)
    (insert (format "%S" lst-with-newlines))
    (switch-to-buffer new-buffer)
    (emacs-lisp-mode)
    (goto-char (point-min))))

(defun fov3-debug-form-definition ()
  "Display the Form Definition."
  (interactive)
  (fov3-debug-data fov3-form-definition "fov3-form-definition"))

(defun fov3-debug-visibility-rules ()
  "Display the Visiblity Rules."
  (interactive)
  (fov3-debug-data fov3-visibility-rules "fov3-visibility-rules"))

(defun fov3-debug-user-code ()
  "Display the User Code."
  (interactive)
  (fov3-debug-data fov3-user-code "fov3-user-code"))

;; -----------------------------------------------------------------------------
;; Loading and Saving

(defun fov3-parse-buffer ()
  "Parse the XML in the buffer."
  (xml-parse-region (point-min) (point-max)))

(defun fov3-before-save ()
  "Convert the internal data structure back to XML ready for
saving."
  (save-excursion
    (erase-buffer)
    (xml-debug-print (cons fov3-service-xml '()))
    (goto-char (point-min))))

;; -----------------------------------------------------------------------------
;; Init

(if fov3-mode-map
    nil
  (setq fov3-mode-map (make-sparse-keymap))
  (define-key fov3-mode-map "\C-c\f" 'fov3-debug-form-definition)
  (define-key fov3-mode-map "\C-c\v" 'fov3-debug-visibility-rules))

(defun fov3--init () 
  (kill-all-local-variables)
  (setq major-mode 'fov3-mode)
  (setq mode-name "Front Office V3")
  (add-hook 'before-save-hook (function fov3-before-save) nil t)
  (use-local-map fov3-mode-map)
  (run-hooks fov3-mode-hook))

(defun fov3-mode ()
  "Major mode for editing Front Office V3 XML templates.

\\{fov3-mode-map}"
  (interactive)
  (fov3--init)
  (setq fov3-service-xml (car (fov3-parse-buffer)))
  (setq fov3-form-definition (car (fov3-select-nodes fov3-service-xml 'FormDefinition)))
  (setq fov3-visibility-rules (car (fov3-select-nodes fov3-service-xml 'VisiblityRules)))
  (setq fov3-user-code (car (fov3-select-nodes fov3-service-xml 'UserCode))))

(provide 'fov3-mode)

;; -----------------------------------------------------------------------------
;; Widget stuff

(require 'tree-widget)
(require 'tree-mode)

(defun xml-attribute-widget (attribute)
  "Generate an editable field for attribute."
  (cond
   ((null attribute) nil)
   (t `(editable-field
	:format ,(concat (format "%S" (car attribute)) ": %v")
	:notify ,(lambda (widget &rest ignore)
		   (message (widget-value widget)))
	,(cdr attribute)))))

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
		    ,@(mapcar (lambda (x) (xml-attribute-widget x)) attributes)
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
