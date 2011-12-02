(define-skeleton service-release-skeleton
  "Inserts an Org Mode release TODO list for a service."
  nil
;;  "*** " (skeleton-read "Enter Service ID: ") " - "
;;  (skeleton-read "Enter Service Description: ") "\n"
  "**** Installation [/]\n"
  "- [ ] Copy Rapid Dev template\n"
  "- [ ] RDT service\n"
  "- [ ] Copy user_code\n"
  "- [ ] Copy AutoUpdate template\n"
  "- [ ] Copy XML\n"
  "- [ ] Add entry to http.properties\n"
  "- [ ] Copy Mayrise XSLT\n"
  "- [ ] Add entry to MayriseServiceConfiguration.xml\n"
  "- [ ] Add Sidebar Links\n"
  "**** Testing [/]\n"
  "- [ ] "
  '(indent-region (point-min) (point-max)))

(define-skeleton mo-mayrise-service-release-skeleton
  "Inserts an Org Mode release TODO list for a Mayrise service."
  nil
  "**** TODO " (upcase (skeleton-read "Enter Service ID: ")) " - "
  (skeleton-read "Enter Service Description: ") "\n"
  "***** Installation [/]\n"
  "- [ ] Copy Rapid Dev template\n"
  "- [ ] Copy AutoUpdate template\n"
  "- [ ] Copy XML\n"
  "- [ ] RDT service\n"
  "- [ ] AutoUpdate service\n"
  "- [ ] Copy user_code\n"
  "- [ ] Add entry to http.properties\n"
  "***** Testing [/]\n"
  "- [ ] "
  '(indent-region (point-min) (point-max)))

(define-skeleton service-installation-skeleton
  "Inserts an Org Mode installation TODO list for a service."
  nil
  "**** Installation [/]\n"
  "- [ ] Copy Rapid Dev template\n"
  "- [ ] RDT service\n"
  "- [ ] Copy user_code\n"
  "- [ ] Copy AutoUpdate template\n"
  "- [ ] Copy XML\n"
  "- [ ] Add entry to http.properties\n"
  "- [ ] Copy Mayrise XSLT\n"
  "- [ ] Add entry to MayriseServiceConfiguration.xml\n"
  "- [ ] Add Sidebar Links"
  '(indent-region (point-min) (point-max)))

(define-skeleton service-release-skeleton
  "Inserts a list of release checkboxes for a Front Office service."
  nil
  "- [ ] CRM3\n"
  "- [ ] CRM4\n"
  "- [ ] CRM1"
  '(indent-region (point-min) (point-max)))

(define-skeleton moc-release-checklist-skeleton
  "Check list for the release of services with Method of Contact changes."
  nil
  "- [ ] Copy RapidDev and mail template\n"
  "- [ ] Copy AutoUpdate template\n"
  "- [ ] Copy user_code\n"
  "- [ ] RDT service\n"
  "- [ ] Auto Update service\n"
  "- [ ] Tested service"
  '(indent-region (point-min) (point-max)))
