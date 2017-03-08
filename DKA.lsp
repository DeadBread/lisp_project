(defvar gram '((S (\a A)) (S (\a)) (S ("eps")) (A (\a)) (A (\a A))))
(defvar lgram '((S (C \c)) (C (A \b)) (C (B \a)) (A (\a)) (A (C \a)) (B (\b)) (B (C \b))))

(defvar gram2 '((A (\a B)) (B (\b B)) (B (\b)) (B (\g G)) (G (\g))))

(defun parse (gr) ( 
	cond ((null gr) nil)
	((cons (trans_right (car gr)) (parse (cdr gr))))))


(defun trans_right (rule) (
	cond ((null (cdr (cadr rule))) (list (car rule) (car (cadr rule)) '("FIN")))
		((list (car rule) (car (cadr rule)) (cdr (cadr rule))))))


(defun trans_left (rule) (
	cond ((null (cdr (cadr rule))) (list '"START" (car (cadr rule)) (car rule)))
		((list (car (cadr rule)) (cadr (cadr rule)) (car rule)))))


(defun my-filter (condp lst) (
     	remove nil
           (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun group_by_connectors (lst element connector) (
	my-filter #'(lambda (x) (and (equal (cadr x) connector) (equal (car x) element))) lst))


(defun group-all (lst) (
	list (remove-duplicates (mapcar #'(lambda (x) (group_by_connectors lst (car x) (cadr x))) lst) :test #'equal ) lst))


(defun to_determined (gram) 
	(let ((grm (car gram)))
		(cond ((null grm) nil)
			((cond ((null (cdr grm)) grm)
				(( append (list (car grm)) (to_determined (list (put_in (process (car grm) gram) (cdr grm)) (cadr gram))))))))))


(defun put_in (rules grm) (
	cond ((null rules) grm)
		((add_set_to_set rules grm))))


(defun add_set_to_set (left right) 
	(print right) (format t '"\n\n")
	(let ( (lst (remove nil (mapcar #'(lambda (x) (set_eql (print x) (print left))) right))))
		;(print left) (print right) (format t '"\n\n")
		(cond ((null lst) (append right (list left)))
			(t right))))


(defun set_eql (left right)
	(equal (set-difference left right :test #'equal) (set-difference right left :test #'equal)))


(defun process (rule gram) (
	cond ((atom rule) nil)
		(( cond ((null (cdr rule)) nil)
			((right_parts (mapcar #'(lambda (x) (caddr x)) rule) gram))))))


(defun right_parts (parts gram)
	(remove nil (reduce #'append (mapcar #'(lambda (r) (right_part r (cadr gram))) parts))))


(defun right_part (left grm)
	(let ((lst (my-filter #'(lambda (x) (equal (car left) (car x))) grm)))
		;(print lst) (print left)
		(cond ((null lst) (list left))
			(T lst))))