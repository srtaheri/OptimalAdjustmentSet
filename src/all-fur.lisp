(loop for effect in (genes-regulated-by-gene (gethash "fur" *genes*))
      do (print-back-door-dagitty (format nil "../data/fur-~A-back-door.tab" (get-symbol effect))
	(gethash "fur" *genes*)
	effect
	*regnet*
	#'get-symbol))
