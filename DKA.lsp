(defvar gram '((S ('\a 'A)) (S ('\a)) (S ('"eps")) (A ('\a)) (A ('\a A))))
(defvar lgram '((S (C \c)) (C (A \b)) (C (B \a)) (A (\a)) (A (C \a)) (B (\b)) (B (C \b))))

(defvar gram2 '((A ('\a 'B)) (B ('\b 'B)) (B ('\b)) (B ('\g' 'G)) (G ('\g))))

(defun parse (gr) ( 
	cond ((null gr) nil)
	((cons (trans_right (car gr)) (parse (cdr gr))))))


(defun trans_right (rule) (
	cond ((null (cdr (cadr rule))) (list (car rule) (car (cadr rule)) '("FIN")))
		((list (car rule) (car (cadr rule)) (cdr (cadr rule))))))


(defun trans_left (rule) (
	cond ((null (cdr (cadr rule))) (list '"START" (car (cadr rule)) (car rule)))
		((list (car (cadr rule)) (cadr (cadr rule)) (car rule)))))


(defun my-filter (condp lst)
     (remove nil
           (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun group_by_connectors (lst element connector) (
	my-filter #'(lambda (x) (and (equal (cadr x) connector) (equal (car x) element))) lst))


(defun group-all (lst) (
	remove-duplicates (mapcar #'(lambda (x) (group_by_connectors lst (car x) (cadr x))) lst) :test #'equal ))

