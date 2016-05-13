;The function returns T if argument x is a member of the argument list Y and NIL otherwise. 
;Works by comparing each element in the list to x. If the list is exhausted and is NIL then x is not in the list.
;test cases:
;	(xmember '1 '(1)) => T
;	(xmember '1 '( (1) 2 3)) => NIL
;	(xmember '(1) '((1) 2 3)) => T
;	(xmember nil nil) => NIL
;	(xmember nil '(nil)) => T
;	(xmember nil '((nil))) => NIL
;	(xmember '(nil) '(1 2 3 (nil))) => T
;	(xmember '(nil) '(nil)) => NIL

(defun xmember (x y)
	(cond
		((null y) 
			NIL)
		((equal x (car y))
			T) 
		(T (xmember x (cdr y)))
		)
	)

;The function moves all elements in the list to one common level keeping the order.
;Works by first checking if atom or another list. If atom, then cons since outer most level. Recursively cons with rest of list and repeat, check if atom.
;If not atom, then is nested list. Recursive calls on elements repeating until is atom (no more nests)
;test cases:
;	(flatten '(a (b c) d)) => (a b c d)
;	(flatten '((((a))))) => (a)
;	(flatten '(a (b c) (d ((e)) f))) => (a b c d e f)

(defun flatten (x)
	(cond
 		((null x) 
 			NIL)
 		((atom (car x)) 
 			(cons 
 				(car x) (flatten (cdr x))
 				)
 			)
 		(T (append 
 			(flatten (car x)) (flatten (cdr x))
 			))
 		)
	)

;The function mixes the elements of L1 and L2 into a single list, by choosing elements from L1 and L2 alternatingly. 
;If one list is shorter than the other, then append all elements from the longer list at the end.
;Works by checking if one of the lists is empty, if so, return the other list. This is done in order to append existing remaining list if one is shorter than the other.
;Appends the first element of each list (converts to a list so it is not a dotted pair), so it alternates. Recursive call on remaining of lists.
;test cases:
;	(mix '(a b c) '(d e f)) => (a d b e c f)
;	(mix '(1 2 3) '(a)) => (1 a 2 3)
;	(mix '((a) (b c)) '(d e f g h)) => ((a) d (b c) e f g h)
;	(mix '(1 2 3) nil) =>(1 2 3)
;	(mix '(1 2 3) '(nil)) =>(1 nil 2 3)

 (defun mix (L1 L2)
	(cond
		((null L1) 
			L2
				L2)
		((null L2) 
			L1
				L1)		
		(T (append 
			(append 
				(list (car L1)) (list (car L2))
				) 
			(mix (cdr L1) (cdr L2))
			))
		)
	) 

;The function splits the elements of L into a list of two sublists (L1 L2), by putting elements from L into L1 and L2 alternatingly.
;If one list is shorter than the other, then append all elements from the longer list at the end.
;Works by first checking if list then checking the rest of the rest. If it is nil then it means that it cannot be split properly so you will 'nil.
;The first list created is every first,3rd,5th, etc element and is done by calling the recursion on these elements.
;The second list created is every 2nd, 4th, 6th, etc element and is done by calling the recursion on these elements.
;These 2 lists are the parameters for another list and therefore falls into one.
;test cases:
;	(split '(1 2 3 4 5 6)) => ((1 3 5) (2 4 6))
;	(split '((a) (b c) (d e f) g h))  => (((a) (d e f) h) ((b c) g))
;	(split '()) => (nil nil)

(defun split (L)
	(if L
		(if (cddr L)
			(list (cons 
				(car L) (car (split (cddr L))))
				(cons 
					(cadr L) (cadr (split (cddr L)))
					)
				)
			(list (list (car L)) (cdr L))
			)
		'(NIL NIL)
		)
	)

;a Lisp function to solve the subset sum problem: given a list of numbers L and a sum S, find a subset of the numbers in L that sums up to S. Each number in L can only be used once.
;Base case: if list is empty then by definition there is no subset sum problem so return NIL.
;Next, there are 3 cases that can happen in subset problem:
;	1) the element by itself is already = to the sum wanted
;		In this case just grab this element and create a list with it and nil
;	2) the element is > than the sum wanted
;		In this case, disregard this element and traverse the rest of the list with recursive call.
;	3) the element is < than the sum wanted
;		In this case, use the element then subtract from the sum the value of this element. Then work with rest of L with recursive call.
;		Also has cond check for null because if returns null then use the rest of list. 
;test cases:
;	(subsetsum '(1 2 3) 5) => (2 3)
;	(subsetsum '(1 5 3) 2) => nil
;	(subsetsum '(1 16 2 8 4) 29) => (1 16 8 4)
;	(subsetsum '(1 1 5 6 8) 10) => (1 1 8)
;	(subsetsum '(1 10 100 1000 10000) 5) => nil

(defun subsetsum (L S)
	(cond
		((null L) 
			NIL)
		((= (car L) S)
			(cons 
				(car L) NIL))	
		((> (car L) S)
			(subsetsum (cdr L) S))
		((< (car L) S)
			(cond 
				((null (subsetsum (cdr L) (- S (car L))))
					(subsetsum (cdr L) S))
				(T (append 
					(list (car L)) (subsetsum (cdr L) (- S (car L)))))))
	)
)
