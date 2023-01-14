(defun find-common-regulators (x y)
  (intersection
   (genes-regulating-gene x)
   (genes-regulating-gene y)
   :test #'fequal))

(defun find-mediators (x y)
  (intersection
   (genes-regulated-by-gene x)
   (genes-regulating-gene y)
   :test #'fequal))

(defun get-shielded-mediators (M U)
  (loop for mediator in M
     unless  (intersection U (genes-regulating-gene mediator) :test #'fequal)
     collect mediator))

(defun find-front-door (filename causes effects)
  ;; TODO:  start with regulator and search for front door.
  (tofile filename
	  (format t "Cause	Effect	Regulators	Mediators~%")
	  (loop for cause in causes
	     do (loop for effect in effects
	     for mediators = (find-mediators cause effect)
	     for regulators = (find-common-regulators cause effect)
	     for shielded-mediators = (get-shielded-mediators mediators regulators)
	     when (and shielded-mediators regulators)
	      do  (format t "~A	~A	~{~A~^ // ~}	~{~A~^ // ~}~%"
		       (get-frame-name cause)
		       (get-frame-name effect)
		       (mapcar #'get-frame-name regulators)
		       (mapcar #'get-frame-name shielded-mediators))))))


(defun get-regulatees  (causes)
  (remove-duplicates
   (loop for cause in causes
	 append (genes-regulated-by-gene cause))
   :test #'fequal))

(defun get-regulators (effects)
  (remove-duplicates
   (loop for effect in effects
	 append (genes-regulating-gene effect)
	 )
   :test #'fequal))

(defun find-causes-of-front-door (filename regulators mediators)
  ;; TODO:  start with regulator and search for front door.
  (tofile filename
	  (format t "Regulator	Mediator	Causes	Effects~%")
	  (loop for regulator in regulators
	     for regulatees = (genes-regulated-by-gene regulator)
	       when regulatees
	     do (loop for mediator in (set-difference mediators regulatees :test #'fequal)
		   for mediator-regulators = (genes-regulating-gene mediator)
		   for mediator-regulatees = (genes-regulated-by-gene mediator)
		   for causes = (intersection regulatees mediator-regulators :test #'fequal)
		   for cause-regulatees = (get-regulatees causes)
		   for effects = (set-difference
				  (intersection regulatees mediator-regulatees
						:test #'fequal)
				  cause-regulatees :test #'fequal)
	     when (and causes effects)
	      do  (format t "~A	~A	~{~A~^ // ~}	~{~A~^ // ~}~%"
		       (get-frame-name regulator)
		       (get-frame-name mediator)
		       (mapcar #'get-frame-name causes)
		       (mapcar #'get-frame-name effects)
		       )))))


(defun find-shielded-confounder (confoundee1 confoundee2 shielded)
  (remove-duplicates
   (loop for confounder in (intersection
			   (genes-regulating-gene confoundee1)
			   (genes-regulating-gene confoundee2)
			   :test #'fequal)
	unless (intersection (genes-regulated-by-gene confounder)
			     shielded :test #'fequal)
	  collect confounder)
  :test #'fequal))

(defun find-napkin-problem (filename)
  ;; X <- U -> W -> R -> X -> Y <- V -> W
  (tofile filename
	  (format t "R	U	V	W	X	Y~%")
	  (loop for W in (gcai '|Genes|)
		for Rs = (genes-regulated-by-gene W)
		when Rs
		  do (loop for R in Rs
			   for Xs = (genes-regulated-by-gene R)
			   when Xs
			     do (loop for X in Xs
				      for Ys = (genes-regulated-by-gene X)
				      when Ys
				      do (loop for Y in Ys
					   for Vs = (find-shielded-confounder W Y (list R X))
					   for Us = (find-shielded-confounder W X (list R Y))
					       for obs = (list R W X Y)
					       when (and R Us Vs W X Y
							 (<= 1 (length Us))
							 (<= 1 (length Vs))
							 (not (find-duplicates obs))
							 (not (intersection Us Vs :test #'fequal))
							 (not (intersection obs Vs :test #'fequal))
							 (not (intersection Us obs :test #'fequal)))
					   do (format t "~A	~{~A~^ // ~}	~{~A~^ // ~}	~A	~A	~A~%"
						      (get-frame-name R)
						      (mapcar #'get-frame-name Us)
						      (mapcar #'get-frame-name Vs)
						      (get-frame-name W)
						      (get-frame-name X)
						      (get-frame-name Y))))))))

(defun find-front-door-regulators-and-causes (filename mediators)
  ;; TODO:  start with regulator and search for front door.
  (tofile filename
	  (format t "Effect	Mediator	Causes	Regulators~%")
	  (loop for mediator in mediators
		do (loop for effect in (genes-regulated-by-gene mediator)
			 for causes = (genes-regulating-gene mediator)
			 for regulators = (set-difference
					   (intersection (get-regulators causes)
							 (genes-regulating-gene effect)
							 :test #'fequal)
					   (cons mediator causes)
					   :test #'fequal)
			 for true-causes = (intersection causes (get-regulatees regulators) :test #'fequal)
			 when (and true-causes regulators (<= 1 (length true-causes)) (<= 1 (length regulators)))
			 do (format t "~A	~A	~{~A~^ // ~}	~{~A~^ // ~}~%"
				    (get-frame-name effect)
				    (get-frame-name mediator)
				    (mapcar #'get-frame-name true-causes)
				    (mapcar #'get-frame-name regulators)
				    )))))

(defun get-shielded-mediators-of-regulator (M U)
  (set-difference
   M
   (genes-regulated-by-gene U)
   :test #'fequal))

(defun find-causes (U M)
  (intersection
   (genes-regulated-by-gene U)
   (genes-regulating-gene M)
   :test #'fequal))

(defun find-effects (U M)
  (intersection
   (genes-regulated-by-gene U)
   (genes-regulated-by-gene M)
   :test #'fequal))

(setq *regulators* (loop for gene in (gcai '|Genes|)
			 when (genes-regulated-by-gene gene)
			   collect gene))

(setq *mediators* (loop for gene in (gcai '|Genes|)
			when (and
			      (genes-regulated-by-gene gene)
			      (genes-regulating-gene gene))
			  collect gene))
(defun print-double-negatives (filename)
  (tofile filename
	  (format t "Inhibitor1	Inhibitor2	Target~%")
	  (loop for tu in (get-class-all-instances '|Transcription-Units|)
		do (loop for inhibitor in (direct-inhibitors tu)
			 when (class-all-type-of-p '|Proteins| inhibitor)
			   do (loop for tu-of-inhibitor in (transcription-units-of-protein inhibitor)
				    do (loop for tf in (direct-inhibitors tu-of-inhibitor)
					     when (and (class-all-type-of-p '|Proteins| tf)
						       (not (fequal tf inhibitor)))
					       do (loop for gene in (transcription-unit-genes tu)
							do (format t "~A	~A	~A~%"
								   (get-frame-name tf)
								   (get-frame-name inhibitor)
								   (get-slot-value gene 'accession-1)
					     ))))))))



(defun print-linear-regulators (filename)
  (tofile filename
	  (format t "TF	Target	Regulation~%")
	  (loop for tu in (get-class-all-instances '|Transcription-Units|)
		do (loop for activator in (direct-activators tu)
			 when (class-all-type-of-p '|Proteins| activator)

			   do (loop for tf in (genes-of-protein activator)
				    do (loop for gene in (transcription-unit-genes tu)
					     do (format t "~A	~A	~A~%"
							(get-slot-value tf 'accession-1)
							(get-slot-value gene 'accession-1)
							"+"))))
		   (loop for inhibitor in (direct-inhibitors tu)
						 when (class-all-type-of-p '|Proteins| inhibitor)
						   do (loop for tf in (genes-of-protein inhibitor)
							    do (loop for gene in (transcription-unit-genes tu)
								     do (format t "~A	~A	~A~%"
										(get-slot-value tf 'accession-1)
										(get-slot-value gene 'accession-1)
										"-")))))))


;; get downstream regulators and ancestors of effects
(defun get-downstream-regulators-and-ancestors-of-effects (filename regulator)
  (tofile filename
	  (format t "Cause	Effect~%")
	  (loop for hop-1 in (genes-regulated-by-gene regulator)
		do (format t "~A	~A~%" (get-frame-name regulator) (get-frame-name hop-1))
		   (loop for ancestors-1 in (genes-regulating-gene hop-1)
			 unless (fequal regulator ancestors-1)
			   do (format t "~A	~A~%" (get-frame-name ancestors-1) (get-frame-name hop-1)))
		   (loop for hop-2 in (genes-regulated-by-gene hop-1)
			 do (format t "~A	~A~%" (get-frame-name hop-1) (get-frame-name hop-2))
			    (loop for ancestors-2 in (genes-regulating-gene hop-2)
				  unless (fequal hop-1 ancestors-2)
				    do (format t "~A	~A~%"  (get-frame-name ancestors-2) (get-frame-name hop-2)))))))


(defun get-3hop-ancestors-of-2hop-descendants (filename regulator)
  (tofile filename
	  (format t "Cause	Effect~%")
	  (loop for hop-1 in (genes-regulated-by-gene regulator)
		do (loop for hop-2 in (genes-regulated-by-gene hop-1)
			 do (loop for ancestors-1 in (genes-regulating-gene hop-2)
				  do (format t "~A	~A~%"
					     (get-slot-value  ancestors-1 'accession-1)
					     (get-slot-value hop-2 'accession-1 ))
				     (loop for ancestors-2 in (genes-regulating-gene ancestors-1)
					   do (format t "~A	~A~%"
						      (get-slot-value ancestors-2 'accession-1)
						      (get-slot-value ancestors-1  'accession-1 ))
					      (loop for ancestors-3 in (genes-regulating-gene ancestors-2)
						    do (format t "~A	~A~%"
							       (get-slot-value ancestors-3 'accession-1)
							       (get-slot-value ancestors-2 'accession-1)))))))))

(defun find-shielded-regulators (x y m)
  (set-difference
   (set-difference
    (intersection
     (genes-regulating-gene x)
     (genes-regulating-gene y)
     :test #'fequal)
    (genes-regulating-gene m)
    :test #'fequal)
   (list x y m)
   :test #'fequal)
   )


(defun find-front-door-of-ko (filename cause)
  ;; TODO:  start with regulator and search for front door.
  (tofile filename
	  (format t "Cause	Effect	Regulators	Mediators~%")
	  (loop for mediator in (genes-regulated-by-gene cause)  ;; children of cause
		unless (fequal cause mediator)                   ;; causes and mediators must be disjoint
		do (loop for effect in (genes-regulated-by-gene mediator)  ;; children of mediator
			 for regulators = (find-shielded-regulators cause effect mediator) ;; filter for regulators that regulate cause and effect, but not mediator
			 unless (or (fequal cause effect)  ;; causes and effects must be disjoint
				    (fequal mediator effect))  ;; mediators and effects must be disjoint
			   do  (format t "~A	~A	~{~A~^ // ~}	~A~%"
				       (get-slot-value cause 'accession-1)
				       (get-slot-value effect 'accession-1)
				       (mapcar #'(lambda (x) (get-slot-value x 'accession-1)) regulators)  ;; there can be multiple regulators for the same cause and effect
				       (get-slot-value mediator 'accession-1))))))


(defun find-napkin-problem-of-ko (filename X)
  ;; X <- U -> W -> R -> X -> Y <- V -> W
  (tofile filename
	  (format t "R	U	V	W	X	Y~%")
	  (let ((ancestors-1 (genes-regulating-gene X)) ;; Looking for Y
		(descendants-1 (genes-regulated-by-gene X))) ;; Looking for R
	    (loop for ancestor-1 in ancestors-1
		  for ancestors-2 = (genes-regulating-gene ancestor-1) ;; Looking for W
		  do (loop for ancestor-2 in ancestors-2
			   for ancestors-3 = (genes-regulating-gene ancestor-2)
			   do (print-napkin X ancestor-1 ancestor-2 ancestors-3 descendants-1))))))

(defun find-V (X R W U Y)
  (loop for V in (set-difference
		  (intersection (genes-regulating-gene W)
				(genes-regulating-gene Y)
				:test #'fequal)
		  (list X R W U Y)
		  :test #'fequal)
	for children = (genes-regulated-by-gene V)
	for parents = (genes-regulating-gene V)
	unless (or (intersection children (list X R U) :test #'fequal)
		   (intersection parents (list X R U) :test #'fequal))
	  collect V)
  )

(defun has-only-unique-elements-p (l)
  (or (null l)
      (and (not (member (car l) (cdr l)))
	   (has-only-unique-elements-p (cdr l)))))




(defun print-napkin (X ancestor-1 ancestor-2 ancestors-3 descendants-1)
  (loop for ancestor-3 in ancestors-3
	for children = (genes-regulated-by-gene ancestor-3)
	for parents = (genes-regulating-gene ancestor-3)
	do (loop for descendant-1 in descendants-1
		 for V = (find-V X ancestor-1 ancestor-2 ancestor-3 descendant-1)
		 unless (or (intersection parents (list ancestor-1 descendant-1) :test #'fequal)
			    (intersection children (list ancestor-1 descendant-1) :test #'fequal)
			    (find-duplicates (list X ancestor-1 ancestor-2 ancestor-3 descendant-1))
			    (not V)
			    )
		   do (format t "~A	~A	~{~A~^ // ~}	~A	~A	~A~%"
			      (get-slot-value ancestor-1 'accession-1)
			      (get-slot-value ancestor-3 'accession-1)
			      (mapcar #'(lambda (vv) (get-slot-value vv 'accession-1)) V)
			      (get-slot-value ancestor-2 'accession-1)
			      (get-slot-value X 'accession-1)
			      (get-slot-value descendant-1 'accession-1)))))


(defun get-confounders (cause effect)
  (mapcar #'get-frame-name (set-difference
			    (intersection
			     (genes-indirectly-regulating-gene-w-caching cause)
			     (genes-indirectly-regulating-gene-w-caching effect)
			     :test #'fequal)
			    (genes-indirectly-regulated-by-gene-w-caching cause)
			    :test #'fequal)))


(defun find-paths (graph start end)
  (let ((queue (list (list start)))
	(paths '()))
    (loop while queue
	  for path = (pop queue)
	  for node = (car (last path))
	  do (if (fequal node end)
		 (push path paths)
		 (loop for next-node in (cdr (assoc node graph :test #'fequal))
		      unless (member next-node path :test #'fequal)
		      do (push (append path (list next-node)) queue))))
    paths))




(defun find-next-path (graph start end found-paths)
  (let ((queue (list (list start)))
	(path '()))
    (loop while queue
	  for path = (pop queue)
	  for node = (car (last path))
	  do (if (and (fequal node end)
		      (not (member path found-paths :test #'fequal)))
		 (return path)
		 (loop for next-node in (cdr (assoc node graph :test #'fequal))
		      unless (member next-node path :test #'fequal)
		      do (push (append path (list next-node)) queue))))))


(defun find-n-paths (graph start end n)
  (let ((queue (list (list start)))
	(paths '())
	(count 0))
    (loop while (and queue (< count n))
	  for path = (pop queue)
	  for node = (car (last path))
	  do (if (fequal node end)
		 (progn (push path paths)
			(incf count))
		 (loop for next-node in (cdr (assoc node graph :test #'fequal))
		      unless (member next-node path :test #'fequal)
		      do (push (append path (list next-node)) queue))))
    paths))


;; (defun find-path (graph start end)
;;   (let ((queue (list (list start)))
;;         (path '()))
;;     (loop while queue
;;           for path = (pop queue)
;;           for node = (car (last path))
;;           do (if (fequal node end)
;;                  (return path)
;;                  (loop for next-node in (cdr (assoc node graph))
;;                       unless (member next-node path)
;;                       do (push (append path (list next-node)) queue))))))

;; (defun find-paths (graph start end visited)
;;   (cond ((fequal start end) (list (list end)))
;;	(t (loop for node in (cdr (assoc start graph :test #'fequal))
;;	     unless (member node visited :test #'fequal)
;;	     append (mapcar (lambda (path) (cons start path))
;;			    (find-paths graph node end (cons start visited))) ))))


;; (defun find-paths (graph start end visited)
;;   (cond ((fequal start end) (list (list end)))
;;	(t (loop for node in (cdr (assoc start graph :test #'fequal))
;;	     unless (member node visited :test #'fequal)
;;	     append (mapcar (lambda (path) (cons node path))
;;			    (find-paths graph node end (cons node visited))) ))))

;(defun find-paths (graph start end)
;  (cond ((fequal start end) '((end)))
;	(t (loop for node in (cdr (assoc start graph :test #'fequal))
;	     append (mapcar (lambda (path) (cons node path))
;			    (find-paths graph node end))))))


(defun create-subnetwork (nodes)
  (loop for node in nodes
	collect (mapcar #'get-frame-name
			(cons node (remove node (genes-regulated-by-gene node) :test #'fequal)))))

(defun find-back-door (cause effect)
  ;;;; The backdoor criterion for  X,Y ∈ V in ADMG A if there is no path from X to Y consisting of bi-directed edges, and there exists a set Z ⊆ V \{X,Y } such that no node is a descendant of X , and Z blocks every path between X and Y that contains an arrow into X [39].
  (let* ((confounders (get-confounders cause effect))
	 (regnet (create-subnetwork confounders)))
    (loop for node in confounders
	  collect (list (find-paths regnet node cause)
			(find-paths regnet node effect)))
    )
  )


(defun get-blattner (gene)
  (get-slot-value gene 'accession-1))

(defun get-symbol (gene)
  (get-slot-value gene 'common-name))

(defun print-back-door (filename cause effect genes n name-func)
  (tofile filename
	  (let* ((confounders (get-confounders cause effect))
		 (regnet (get-full-regnet genes)))
	    (loop for node in confounders
		  do (loop for path in (find-n-paths regnet node cause n)
			   do (format t "~{~A~^->~}~%"
				      (mapcar name-func path)))
		     (loop for path in (find-n-paths regnet node effect n)
			   do (format t "~{~A~^->~}~%"
				   (mapcar name-func path)))))))
(defun get-full-regnet (genes)
  (loop for gene in genes
	for regulatees = (remove gene (genes-regulated-by-gene gene) :test #'fequal)
	when regulatees
	  collect (mapcar #'get-frame-name (cons gene regulatees))))
