;Made by
;Zhikov Vasiliy
;group 324

;variant = RG -> DKA (dsm)

;how to use
;provide RG as a list of rules to the function "grammar"
;rule of RG looks like (A (\b C)) where A - left part og the rule, (\b C) - right part og the rule
;Terminal symbols should be written as \t (lowercase latin letters)


;to process NSM (non-deterministic state machine) use function "automat"


(defvar gram '((H (\a A)) (H (\a)) (H ("eps")) (A (\a)) (A (\a A))))

;(defvar lgram '((H (C \c)) (C (A \b)) (C (B \a)) (A (\a)) (A (C \a)) (B (\b)) (B (C \b))))

(defvar gram2 '((H (\a B)) (B (\b B)) (B (\b)) (B (\g G)) (G (\g))))

(defvar gram3 '((H (\a D)) (H (\b B)) (B (\a C)) (B (\b C)) (C (\a D)) (C (\b D)) (C ("eps")) (D (\a D)) (D (\b D))))

(defvar gram4 '((H (\a A)) (H (\b B)) (A (\b C)) (C (\b B)) (C (\a A)) (C ("eps")) (B (\a C))))



(defvar NKA '((H \a B) (B \b B) (B \b C) (C \c C) (C \c D) (C \d "FIN") (D \d "FIN")))

(defvar NKA2 '((H \a A) (A \a A) (A \a S) (H \a B) (H \b B) (B \a B) (B \a A) (B \b S)))

(defvar NKA3 '((H \a B) (H \a C) (C \b B) (C \c S) (B \b C) (B \b S)))

(defvar NKA4 '((H \a A) (A \a A) (A \b S) (H \a B) (B \b B) (B \a S)))

(defvar test1 '((H \a A) (B \a B) (A \a A) (B \b B) (A \b S) (B \a S)))


(defvar gr '((H = \a N \| \b N) (N = \c N \| \d)))


;this group of functions doens nothing except transposing input to the inner representation

(defun transpose_grammar (bad_gram)
	(cond ((null bad_gram) nil)
	((append (transpose_rule (car bad_gram)) (transpose_grammar (cdr bad_gram))))))

(defun transpose_rule (rule)
	(transpose_both_parts 
					(car rule)
					(cddr rule)))


(defun transpose_both_parts (left right)
	; (print right)
	(cond ((null right) nil)
		((or (null (cdr right)) (null (cddr right))) (list (make_simple_rule 
																left
																right)))
		(T (cond ((equal (caddr right) '\|) (cons
												(make_simple_rule 
															left 
															(list (car right) (cadr right)))
												(transpose_both_parts
															left
															(cdddr right))))
				(T(cons
						(make_simple_rule
								left
								(list (car right)))
						(transpose_both_parts
									left 
									(cddr right))))))))



(defun make_simple_rule (left right)
	(list left right))



;this function you should use to get the deterministic state machine from the grammar
(defun grammar (gram) (trasmit_to_final(remove_unattainable (to-determined (group-all (parse (transpose_grammar gram)))))))


;this function you should use to get the deterministic state machine from the non-determenistic one
(defun automat (nka) (trasmit_to_final (remove_unattainable (to-determined (group-all nka)))))





;next group of functions transfers RG to NSM


;parses grammar to the separate rules
(defun parse (gr) ( 
	cond ((null gr) nil)
	((cons (trans_right (car gr)) (parse (cdr gr))))))


;transposes every grammar rule to the automat rule.
(defun trans_right (rule) (
	cond ((null (cdr (cadr rule))) (list (car rule) (car (cadr rule)) '"FIN"))
		((list (car rule) (car (cadr rule)) (cadr (cadr rule))))))


;same for left-sided grammars
; (defun trans_left (rule) (
; 	cond ((null (cdr (cadr rule))) (list '"START" (car (cadr rule)) (car rule)))
; 		((list (car (cadr rule)) (cadr (cadr rule)) (car rule)))))



;nwxt group implements grouping rules of nsm for the purpose of later processing

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





;next group constructs DSM out of NSM


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
	(cond ((member 
				rule 
				watched 
				:test #'set-equal) 
			unwatched)
		(T (adjoin 
				rule 
				unwatched
				:test #'set-equal))))


;an "equal" function for sets
(defun set-equal (left 
				  right)
	(and
		(null (set-difference left right :test #'equal))
		(null (set-difference right left :test #'equal))))


;this function takes one rule to process and list of all rules of nsm should return set of the rules derived from the one processed rule
(defun process ( rule
				 all_rules )
	;(print rule)
	;"simple" rules doesn't need any processing
	(cond ((null (cdr rule)) nil)
		(T ( let ((con (make_connectors_set rule)))
				(remove nil (process_vertices
										(make_vertices_set 
														rule
														con)
										all_rules))))))



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
	(remove-duplicates (mapcar 
		#'caddr 
		(my-filter
				#'(lambda (sub_rule) (cond ((null (cdr sub_rule)) nil)
										(T(equal (cadr sub_rule) connector))))
				rule))
	:test #'equal))


;this function implements processing for all the new vertices we've made previously
;it decomposes vertices to single letters and reduces results of processing each of them
;it returns list of all the rules derived from all of the vertices
(defun process_vertices (vertices 
						 all_rules)
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
	(cond ((null (cdr vertex)) nil)
		((reduce 
				#'append 
				(mapcar
					#'(lambda (letter) (process_letter
													letter
													all_rules))
					vertex)))))


;return list of rules derived from one particular letter. 
;if there is no rule for the letter - it return double list of this letter
(defun process_letter (letter 
					   all_rules)
	(let ((lst (my-filter 
					#'(lambda (rule) (equal
										(car rule)
										letter))
					all_rules)))
		(cond ((null lst) (list (list letter)))
			(T lst))))



;next group is made to delete unattainable vertices from DSM


;removes unattainable rules from dsm
(defun remove_unattainable (dsm)
	(let ((begin (my-filter
						#'(lambda (rule) (same_start 
												rule
												'H))
						dsm)))
		
		(remove_unattainable_2
						begin
						dsm)))


;recursive funtion with accumulator
(defun remove_unattainable_2 (good 
							  all)
	(let ((derived (pick_derived 
							good 
							all)))
		(cond ((null (set-difference
								derived
								good 
								:test #'set-equal)) 
				good)
			((remove_unattainable_2
				(add_set_to_set
							good
							derived)
				all)))))


;adding one set to another (why did I write that? It's just "union")
(defun add_set_to_set (left right)
	(cond ((null right) left)
		(T (add_set_to_set
						(adjoin 
							(car right)
							left
							:test #'set-equal)
						(cdr right)))))


;takes set of the "good" rules and returns set of rules derived from them
(defun pick_derived (good
					 all)
	(remove-duplicates (reduce 
		#'append
		(mapcar
			#'(lambda (good_rule) (derived_by_rule
												good_rule
												all))
			good))))


(defun derived_by_rule (good_rule all)
	(my-filter
			#'(lambda (rule) (single_derived_by_rule
												good_rule
												rule))
			all))


(defun single_derived_by_rule (good_rule rule)
	(is_in_right_part
					(get_left_part rule)
					good_rule))
	

;returns left part of the rule as set of letters
(defun get_left_part (rule)
	(remove-duplicates (mapcar 
							#'car 
							rule)))


;checks if this set of letters appears in the right part of any rule in dsm
(defun is_in_right_part (left rule)
	(let ((con (make_connectors_set rule)))
		(let ((vert_set (make_vertices_set
										rule
										con)))
			(member 
				left 
				vert_set
				:test #'set-equal))))



;checks if all the sub_rules in this rule ctart with "first letter"
(defun SAME_START (RULE FIRST_LETTER)
	(cond ((null rule) T)
		((and (equal first_letter (car (car rule))) (same_start (cdr rule) first_letter)))))



;formats the output to the presenable form
(defun trasmit_to_final (dsm)
	(cond ((null dsm) nil)
		((null (cdr (car dsm))) (cons 
									(car dsm) 
									(trasmit_to_final (cdr dsm))))
		(T (append 
				(finalization (car dsm))
				(trasmit_to_final (cdr dsm))))))


(defun finalization (rule)
	(let ((left (get_left_part rule))
		(con (make_connectors_set rule)))
		
		(my-filter 
				#'(lambda (x) (cadr x))
				(mapcar
						#'(lambda (connector) (new_rule
													left
													connector
													rule))
						con))))



(defun new_rule (left
				 connector
				 rule)
	(let ((right (get_right_parts_by_connector
											connector
											rule)))
		(list 
			left
			connector
			right)))