;; -----------------------------------------------------------------------------
;; Front Office V3 Mode Testing code

(load-file "front-office3.el")

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

(setq table16
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
      "))

;;(fov3-node-p table18) ;; t
;;(fov3-node-p row5) ;; t
;;(fov3-node-p column0) ;; t

;;(fov3-node-eq 'Row row5) ;; t

;; (fov3-node-parent-p row5) ;; t
;; (fov3-node-parent-p table18) ;; t
;; (fov3-node-parent-p column0) ;; nil

;;- first element symbol
;;- second element alist
;;- third element string
;;- fourth element child node

(fov3-collect-children table18)
(fov3-collect-children row5)
(car (fov3-collect-children row5))
(car (cdr (fov3-collect-children row5)))
(fov3-collect-children fov3-service-xml)

(setq fov3-service-xml (car (xml-parse-file "MOpestCont.xml")))

(fov3-debug-data fov3-service-xml)
(fov3-debug-data (fov3-select-nodes fov3-service-xml 'Table))
(fov3-debug-data (fov3-select-nodes table18 'Column))
(fov3-debug-data (fov3-select-nodes table16 'Row))
(fov3-debug-data (fov3-select-nodes fov3-service-xml 'SelfServe))
(fov3-debug-data (fov3-select-nodes fov3-service-xml 'VisibilityRules))

(fov3-debug-data (fov3--strip-newlines fov3-service-xml))
