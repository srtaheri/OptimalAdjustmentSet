(load "regnet-utils.lisp")
(print-back-door "../data/fur-sodA-back-door.tab"
		 (gethash "fur" *genes*)
		 (gethash "sodA" *genes*)
		 (get-class-all-instances '|Genes|)
		 1
		 #'get-symbol)
