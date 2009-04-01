;; -----------------------------------------------------------------------------
;; Front Office V3 Mode Testing code

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
(fov3-collect-children service-xml)

(setq service-xml (xml-parse-file "MOpestCont.xml"))
(fov3-select-nodes service-xml 'Table)
;;(print service-xml)
