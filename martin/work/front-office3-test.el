;; -----------------------------------------------------------------------------
;; Front Office V3 Mode Testing code

(load-file "front-office3.el")

(progn
  (setq table18
	(fov3-remove-newlines
	 '(Table ((id . "table18") (name . "tblLocationOther") (repeat . "N") (allowAdd . "N") (allowSelect . "N") (allowEdit . "N") (rowPos . "5") (visibleOnload . "N") (visibilityRuleType . "S") (visibilityRuleName . "selLocationOther") (autoPlainText . "N") (tableSummary . "") (restrictSSAccess . "N")) "
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
      ")))

(setq row5
      (fov3-remove-newlines
       '(Row ((id . "5")) "
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
        ")))

(setq column0
      (fov3-remove-newlines
       '(Column ((id . "0")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "right") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (PlainText ((id . "plaintext12") (associatedControl . "")) "Other Location") "
          ")))

(setq table16
      (fov3-remove-newlines
       '(Table ((id . "table16") (name . "tblPestDetails") (repeat . "N") (allowAdd . "N") (allowSelect . "N") (allowEdit . "N") (rowPos . "1") (visibleOnload . "Y") (visibilityRuleType . "") (visibilityRuleName . "") (autoPlainText . "N") (tableSummary . "") (restrictSSAccess . "N")) "
        " (Row ((id . "1")) "
          " (Column ((id . "0")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "right") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (PlainText ((id . "plaintext5") (associatedControl . "")) "Status") "
          ") "
          " (Column ((id . "1")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "left") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (Control ((id . "textbox9") (name . "txtStatus") (type . "TextBox") (mand . "N") (workflow . "Y") (searchable . "N") (postback . "N") (visibleOnload . "Y") (visibilityRuleType . "") (visibilityRuleName . "") (mapvariable . "")) "
              " (CtlTextBox ((def . "") (ctlsize . "20"))) "
            ") "
          ") "
        ") "
        " (Row ((id . "2")) "
          " (Column ((id . "0"))) "
          " (Column ((id . "1")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "left") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (Control ((id . "button6") (name . "btnPopulatePests") (type . "Button") (mand . "N") (workflow . "Y") (searchable . "N") (postback . "N") (visibleOnload . "Y") (visibilityRuleType . "") (visibilityRuleName . "") (mapvariable . "")) "
              " (BOEvent ((BOControlName . "populatePests"))) "
              " (CtlButton ((type . "H") (text . "Populate Pests") (image . "") (hoverimage . "") (calltype . "B") (disableOnClick . "Y")) "
              ") "
            ") "
          ") "
        ") "
        " (Row ((id . "3")) "
          " (Column ((id . "0")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "right") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (PlainText ((id . "plaintext1") (associatedControl . "")) "Type of Pest") "
          ") "
          " (Column ((id . "1")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "left") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (Control ((id . "select10") (name . "selPest") (type . "Select") (mand . "N") (workflow . "Y") (searchable . "N") (postback . "N") (visibleOnload . "Y") (visibilityRuleType . "") (visibilityRuleName . "") (mapvariable . "")) "
              " (BOEvent ((BOControlName . "getPestCost"))) "
              " (CtlSelect ((def . "") (populateFromBO . "Y")) "
              ") "
            ") "
          ") "
        ") "
        " (Row ((id . "4")) "
          " (Column ((id . "0")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "right") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (PlainText ((id . "plaintext7") (associatedControl . "")) "Location") "
          ") "
          " (Column ((id . "1")) "
            " (Style ((font-name . "") (font-color . "") (font-size . "") (backcolor . "") (align . "left") (font-bold . "N") (font-underline . "N") (font-italic . "N"))) "
            " (Control ((id . "select11") (name . "selLocation") (type . "Select") (mand . "N") (workflow . "Y") (searchable . "N") (postback . "Y") (visibleOnload . "Y") (visibilityRuleType . "") (visibilityRuleName . "") (mapvariable . "")) "
              " (CtlSelect ((def . "") (populateFromBO . "N")) "
                " (Option ((text . "House") (val . "House"))) "
                " (Option ((text . "Flat") (val . "Flat"))) "
                " (Option ((text . "Garage") (val . "Garage"))) "
                " (Option ((text . "Other") (val . "Other"))) "
              ") "
            ") "
          ") "
        ") "
      ")))
(setq fov3-service-xml (fov3-remove-newlines (car (xml-parse-file "CHSCISNR01.xml"))))
(setq mo-pest-cont-xml (fov3-remove-newlines (car (xml-parse-file "MOpestCont.xml"))))
(setq fov3-user-code (fov3-select-nodes mo-pest-cont-xml 'UserCode)))

;;(fov3-node-p table18) ;; t
;;(fov3-node-p row5) ;; t
;;(fov3-node-p column0) ;; t

;;(fov3-node-eq 'Row row5) ;; t

;;(fov3-node-parent-p row5) ;; t
;;(fov3-node-parent-p table18) ;; t
;;(fov3-node-parent-p column0) ;; nil
;;(fov3-node-parent-p table16) ;; t

(fov3-debug-data fov3-service-xml)
(fov3-debug-data (fov3-get-children fov3-service-xml))

(fov3-debug-data (fov3-select-nodes table18 'Column))
(fov3-debug-data (fov3-select-nodes table16 'Row))
(fov3-debug-data (fov3-select-nodes fov3-service-xml 'Table))
(fov3-debug-data (fov3-select-nodes fov3-service-xml 'SelfServe))
(fov3-debug-data (fov3-select-nodes fov3-service-xml 'VisibilityRules))

(fov3-debug-data (fov3-select-node-with-attr mo-pest-cont-xml 'Row '(id . "0")))
(fov3-debug-data (fov3-select-node-with-attr mo-pest-cont-xml 'PlainText '(id . "plaintext7")))
(fov3-debug-data (fov3-select-node-with-attr mo-pest-cont-xml 'Table '(id . "table16")))
(fov3-debug-data (fov3-select-node-with-attr fov3-service-xml 'Table '(name . "tblGeneralEnquiry8")))
(fov3-debug-data (fov3-select-node-with-attr fov3-service-xml 'Control '(name . "txtChildcareArea")))
(fov3-debug-data (fov3-select-node-with-attr table16 'Column '(id . "0")))

(fov3-debug-data table16)
(fov3-debug-data (fov3-get-tree table16 '(5 2)))

(fov3-table-count)
(fov3-row-count table16)
(fov3-column-count (car (fov3-select-nodes table16 'Row)))

(fov3-debug-data (fov3-set-user-code-body (car (fov3-select-nodes (fov3-get-user-code "2") 'CodeBody)) "Testing"))

(fov3-node-set-attribute (fov3-select-node-with-attr mo-pest-cont-xml 'Table '(id . "testing")) 'id "table16")
(fov3-debug-data mo-pest-cont-xml)

(eq (fov3-get-children table16)
    (fov3-get-children table16))