(defvar gram '((S (\a A)) (S (\a)) (S ("eps")) (A (\a)) (A (\a A))))

(defvar lgram '((S (C \c)) (C (A \b)) (C (B \a)) (A (\a)) (A (C \a)) (B (\b)) (B (C \b))))

(defvar gram2 '((S (\a B)) (B (\b B)) (B (\b)) (B (\g G)) (G (\g))))

(defvar NKA '((A \a (B)) (B \b (B)) (B \b (C)) (C \c (C)) (C \c (D)) (C \d ("FIN")) (D \d ("FIN"))))


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


;the main function to build a determined automat out of the undetermined
(defun to_determined (gram) (
	let ((grm (car gram)))
		(cond ((null grm) nil)
			((cond ((null (cdr grm)) grm)
				(( append (list (car grm)) (to_determined (remove-duplicates (list (put_in (process (car grm) gram) (cdr grm)) (cadr gram)) :test #'equal)))))))))


;function to insert the list of the new rules to the grammar
(defun put_in (rules grm) (
	cond ((null rules) grm)
		((add_set_to_set rules grm))))


;implements operation for adding an element to the list treating in as set
(defun add_set_to_set (left right) (
	;print right) (format t '"\n\n") (
	let ( (lst (remove nil (mapcar #'(lambda (x) (set_eql x left)) right))))
		;(print left) (print right) (format t '"\n\n")
		(cond ((null lst) (append right (list left)))
			(t right))))


;an equal operator for sets
(defun set_eql (left right) (
	equal (set-difference left right :test #'equal) (set-difference right left :test #'equal)))


;processing the rule to build the new rules (according to the algorithm)
(defun process (rule gram) (
	cond ((atom rule) nil)
		((cond ((null (cdr rule)) nil)
			((cond ((listp (car (car rule))) (process_big_rule rule gram))
				((right_parts (mapcar #'(lambda (x) (list (cadr x) (caddr x))) rule) gram))))))))


(defun process_big_rule (big_rule gram) (
	mapcar #'(lambda (x) '(process x gram)) big_rule))


;
(defun right_parts (parts gram) (
	let ((connector_list (remove-duplicates (mapcar #'(lambda (x) (car x)) parts))))
		(right_parts_2 connector_list parts (cadr gram))))
		;remove nil (reduce #'append (mapcar #'(lambda (r) (right_part (car r) (cdr r) (cadr gram))) parts))))

(defun right_parts_2 (connector_list parts gram) (
	cond (( null connector_list) nil)
		((let ((letters (mapcar #'(lambda (x) (cadr x)) (my-filter #'(lambda (x) (equal (car x) (car connector_list))) parts))))
			(cons (pick letters gram) (right_parts_2 (cdr connector_list) parts gram))))))


(defun pick (letters gram) (
	reduce #'append (mapcar #'(lambda (x) (right_part x gram)) letters)))


(defun right_part (left grm) (
	let ((lst (my-filter #'(lambda (x) (equal (car left) (car x))) grm)))
		;(print lst) (print left)
		(cond ((null lst) (list left))
			(T lst))))










(defun delete_unattainable (gram)
	(my-filter #'(lambda (rule) (is_unattainable rule gram)) gram))



(defun is_unattainable (rule gram) (
	cond ( (equal (car (car rule)) 'S) T)
		((cond ( (cdr rule) (mult_unattainable rule gram))
			 ( (unattainable_left (car (car rule)) gram))))))


(defun mult_unattainable (rule gram)
	(cond ((not (same_start rule (car (car rule)))) T)
		((unattainable_left (car (car rule)) (remove rule gram)))))


;check or and "and
(defun unattainable_left (left gram)
	(reduce #'(lambda (x y) (or x y)) (mapcar #'(lambda (x) (is_in_right_part left x)) (my-filter #'(lambda (x) (null (cdr x))) gram))))


(defun is_in_right_part (letter rule) (
;	cond ((cdr rule) (right_part_multi_rule letter rule))
	equal (car (caddr (car rule))) letter))


(defun SAME_START (RULE FIRST_LETTER)
	(cond ((null rule) T)
		((and (equal first_letter (car (car rule))) (same_start (cdr rule) first_letter)))))