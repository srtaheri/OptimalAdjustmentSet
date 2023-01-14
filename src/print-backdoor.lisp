(compile-file "regnet-utils.lisp")
(load "regnet-utils")
(print-back-door-dagitty "../data/fur-sodA-back-door.tab"
		 (gethash "fur" *genes*)
		 (gethash "sodA" *genes*)
		 (get-class-all-instances '|Genes|)
		 1
		 #'get-symbol)
