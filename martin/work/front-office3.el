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
  "Get the children of the NODE."
  (cond
   ((null node) ())
   ((fov3-node-p (car node))
    (cons (car node) (fov3-collect-children (cdr node))))
   (t (fov3-collect-children (cdr node)))))

(defun fov3-get-children (node)
  "Get the children of the NODE."
  (car (fov3-collect-children node)))

;; Why doesn't this nested call work?
(fov3-get-children (fov3-get-children table18))

(equal row5 (fov3-get-children table18))

(defun fov3-newline-p (string)
  "Does STRING start with a newline?"
  (if (stringp string)
      (= (aref string 0) ?\n)
    nil))

;; Need to check the children as well. The name may not match the parent, but it
;; may match one of the children.
(defun fov3-collect-nodes (l name &optional collector)
  "Collect all of the nodes with NAME in list L."
  (let ((col (or collector '())))
    (cond
     ((null l) col)
     ((and (fov3-node-p (car l))
	   (eq name (xml-node-name (car l))))
      (cond
       ((fov3-node-parent-p (car l))
	(fov3-collect-nodes (cdr l) name
			    (cons col (fov3-collect-nodes (fov3-collect-children (car l)) name))))
       (t (cons col (car l) (fov3-collect-nodes (cdr l) name)))))
      (t
      (fov3-collect-nodes (cdr l) name col)))))

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

;; -----------------------------------------------------------------------------
;; Testing code

(setq table18 '(Table ((id . "table18") (name . "tblLocationOther") (repeat . "N") (allowAdd . "N") (allowSelect . "N") (allowEdit . "N") (rowPos . "5") (visibleOnload . "N") (visibilityRuleType . "S") (visibilityRuleName . "selLocationOther") (autoPlainText . "N") (tableSummary . "") (restrictSSAccess . "N")) "
        " (Row ((id . "5")) "
          " (Column ((id . "0")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "right") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (PlainText ((id . "plaintext12") (associatedControl . "")) "Other Location") "
          ") "
          " (Column ((id . "1")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "left") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (Control ((id . "textarea1") (name . "txaOther") (type . "TextArea") (mand . "N") (workflow . "Y") (searchable . "N") (postback . "N") (visibleOnload . "Y") (visibilityRuleType . "") (visibilityRuleName . "") (mapvariable . "")) "
              " (CtlTextArea ((def . "") (rownum . "4") (colnum . "20"))) "
            ") "
          ") "
        ") "
      "))

(setq row5 '(Row ((id . "5")) "
          " (Column ((id . "0")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "right") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (PlainText ((id . "plaintext12") (associatedControl . "")) "Other Location") "
          ") "
          " (Column ((id . "1")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "left") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (Control ((id . "textarea1") (name . "txaOther") (type . "TextArea") (mand . "N") (workflow . "Y") (searchable . "N") (postback . "N") (visibleOnload . "Y") (visibilityRuleType . "") (visibilityRuleName . "") (mapvariable . "")) "
              " (CtlTextArea ((def . "") (rownum . "4") (colnum . "20"))) "
            ") "
          ") "
        "))

(setq column0 '(Column ((id . "0")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "right") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (PlainText ((id . "plaintext12") (associatedControl . "")) "Other Location") "
          "))

;;(fov3-node-p table18) ;; t
;;(fov3-node-p row5) ;; t
;;(fov3-node-p column0) ;; t

;;(fov3-node-eq 'Row row5) ;; t

;;(fov3-node-parent-p row5) ;; t
;;(fov3-node-parent-p table18) ;; t
;;(fov3-node-parent-p column0) ;; nil

;;(fov3-collect-nodes table18 'Column)

;;- first element symbol
;;- second element alist
;;- third element string
;;- fourth element child node

(fov3-collect-children table18)
(fov3-collect-children row5)
(car (fov3-collect-children row5))
(car (cdr (fov3-collect-children row5)))

;;(setq service-xml (xml-parse-file "MOpestCont.xml"))
;;(print service-xml)
;;(fov3-read-node service-xml)

;; xml.el has functions for getting attributes of a node, and getting children of a node
;; xml-get-children should provide ideas for building a list of matched nodes. 
;; (xml-get-children service-xml 'Controls)

(xml-node-name row5)