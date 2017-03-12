(defvar gram '((S (\a A)) (S (\a)) (S ("eps")) (A (\a)) (A (\a A))))

(defvar lgram '((S (C \c)) (C (A \b)) (C (B \a)) (A (\a)) (A (C \a)) (B (\b)) (B (C \b))))

(defvar gram2 '((S (\a B)) (B (\b B)) (B (\b)) (B (\g G)) (G (\g))))

(defvar NKA '((A \a B) (B \b B) (B \b C) (C \c C) (C \c D) (C \d "FIN") (D \d "FIN")))


;parses grammar to the separate rules
(defun parse (gr) ( 
	cond ((null gr) nil)
	((cons (trans_right (car gr)) (parse (cdr gr))))))


;transposes every grammar rule to the automat rule.
(defun trans_right (rule) (
	cond ((null (cdr (cadr rule))) (list (car rule) (car (cadr rule)) '("FIN")))
		((list (car rule) (car (cadr rule)) (cdr (cadr rule))))))


;same for left-sided grammars
(defun trans_left (rule) (
	cond ((null (cdr (cadr rule))) (list '"START" (car (cadr rule)) (car rule)))
		((list (car (cadr rule)) (cadr (cadr rule)) (car rule)))))


;realisation of the "filter" functional
(defun my-filter (condp lst) (
     	remove nil
           (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


;groups the rules with the same left parts and connectors to the lists
(defun group_by_connectors (lst element connector) (
	my-filter #'(lambda (x) (and (equal (cadr x) connector) (equal (car x) element))) lst))


;implements the "group" function to the whole grammar. Also saves the unprocessed grammar as "cadr"
(defun group-all (lst) (
	list (remove-duplicates (mapcar #'(lambda (x) (group_by_connectors lst (car x) (cadr x))) lst) :test #'equal ) lst))


;nsm contains grouped set of rules and just unstructured set of rules as cadr
(defun to-determined (nsm)
	(to-determined_2 () (car nsm) (cadr nsm)))


(defun to-determined_2 (watched unwatched all_rules)
	;(print unwatched) (print watched)
	(cond ((null unwatched) watched)
			((append (to-determined_2 
									(cons (car unwatched) watched) 
									(update_unwatched 
													(cons (car unwatched) watched) 
													(cdr unwatched)
													(process (car unwatched) all_rules))
									all_rules)))))


;adding product of "process" function to the unwatched list correctly
(defun update_unwatched (watched 
						unwatched
						to_add)
		(cond ((null to_add) unwatched)
			((update_unwatched 
							watched 
							(put_in watched unwatched (car to_add)) 
							(cdr to_add)))))


;adding rule to the "unwatched" set
(defun put_in ( watched
				unwatched
				rule )
	(cond ((member rule watched :test equal) unwatched)
		((adjoin rule unwatched))))




;this function takes one rule to process and list of all rules of nsm should return set of the rules derived from the one processed rule
(defun process ( rule
				 all_rules )
	;(print rule)
	;"simple" rules doesn't need any processing
	(cond ((null (cdr rule)) nil)
		(T ( let ((con (make_connectors_set rule)))
				(process_vertices
						(make_vertices_set 
										rule
										con)
						all_rules)))))



;makes the set of rule connectors
(defun make_connectors_set (rule)
	(remove-duplicates (mapcar #'cadr rule)))


;two next functions create set of vertices to be processed
(defun make_vertices_set (rule con)
	(remove nil (mapcar 
		#'(lambda (connector) (get_right_parts_by_connector 
														connector
														rule))
		con)))


(defun get_right_parts_by_connector (connector
									 rule)
	(mapcar 
		#'caddr 
		(my-filter
				#'(lambda (sub_rule) (cond ((null (cdr sub_rule)) nil)
										(T(equal (cadr sub_rule) connector))))
				rule))) 


;this function implements processing for all the new vertices we've made previously
;it decomposes vertices to single letters and reduces results of processing each of them
;it returns list of all the rules derived from all of the vertices
(defun process_vertices (vertices all_rules)
	(cond ((null vertices) nil)
		(T (cons 
				(process_vetrex 
							(car vertices)
							all_rules)
				(process_vertices
							(cdr vertices)
							all_rules)))))


;this decomposes vertex into single letter, processes every letter and sets the result together
(defun process_vetrex (vertex all_rules)
	(reduce 
			#'append 
			(mapcar
				#'(lambda (letter) (process_letter
												letter
												all_rules))
				vertex)))


;return list of rules derived from one particular letter. 
;if there is no rule for the letter - it return double list of this letter
(defun process_letter (letter all_rules)
	(let ((lst (my-filter 
					#'(lambda (rule) (equal
										(car rule)
										letter))
					all_rules)))
		(cond ((null lst) (list (list letter)))
			(T lst))))







(defun SAME_START (RULE FIRST_LETTER)
	(cond ((null rule) T)
		((and (equal first_letter (car (car rule))) (same_start (cdr rule) first_letter)))))