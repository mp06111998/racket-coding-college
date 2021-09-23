(fri(mul (s(set (zz 1)(zz 2)))(s(set (zz 3)(zz 4))))null)
(egual? (struct-info(zz 1))(struct-info(zz 2)))
(fri(is-proper-seq?(.. (zz 1)(.. (zz 2)(..(zz 4)(empty)))))null)
(fri(rounding(qq(zz 18)(zz 4)))null)

(fri(true)null)
(fri(false)null)
(fri(zz 1)null)
(fri(zz 5)null)
(fri(qq (zz 1)(zz 5))null)
;more zaporedje
((fri(s (set 1 2 3)))null)

(fri (vars "a" (zz 1) (zz 2))null)

(fri (valof "a")null)
(fri (vars "a" 2 (valof "a"))null)
(fri (vars "a" 1 (valof "a"))null)
(fri (vars (list "a" "b") (list 1 2) (valof "a"))null)
(fri (vars (list "a" "b") (list 1 2) (valof "b"))null)
(fri(vars (list "a" "b") (list (zz 1) (zz 2)) (valof "b")null)

(fri(if-then-else (false) (zz 11) (zz 22))null)
(fri (is-bool? (true))null)
(fri (is-seq? (.. (zz 11)(zz 2)))null)

(fri (add (zz 5) (zz 8))null)
(fri(add (qq (zz 1) (zz 2)) (qq (zz -3) (zz 4)))null)
(fri(add (zz 5) (qq (zz -3) (zz 4)))null)
(fri(add (true)(false))null)
(fri(add (true)(false))null)
(fri(add (true)(true))null)
(fri(add (false)(false))null)
(fri (add (zz 5) (zz -8))null)
;more zaporedje
(fri (add (s (set 1 2 3)) (s (set 1 2 3)))null)
(fri (add (s (set 1 2 3)) (s (set 1 2 5 3 4)))null)

(fri (mul (zz 5) (zz 8))null)
(fri (mul (zz 5) (zz -2))null)
(fri (mul (true) (true))null)
(fri (mul (true) (false))null)
(fri(mul (zz 5) (qq (zz -1) (zz 4)))null)
(fri(mul (zz 5) (qq (zz 1) (zz 4)))null)
;more kartezicni produkt

(fri (leq? (zz 8)(zz 6))null)
(fri (leq? (true)(false))null)
(fri (leq? (false)(false))null)
(fri (leq? (qq (zz 5) (zz 2))(zz 2))null)
(fri (leq? (qq (zz 5) (zz 2))(qq (zz 5) (zz 2)))null)
;more zaporedje
(fri(leq? (s (set 1)) (s (set 1 2 3)))null)

;rounding
(fri(=? (true) (false))null)
(fri(=? (zz 2) (zz 3))null)
(fri(=? (zz 3) (zz 3))null)

(fri (left (qq (zz 11)(zz 22)))null)
;more zaporedje
(fri(left (s (set 1 2 3)))null)

(fri (right (qq (zz 11)(zz 22)))null)
;more zaporedje
(fri(right (s (set 1 2 3)))null)

(fri(~ (zz 2))null)
(fri(~ (qq (zz 2) (zz 4)))null)
(fri(~ (true))null)
(fri(~ (false))null)

;more za zaporedje
(fri(all? (s (set 1 2 3 4 (true) (false))))null)
(fri(all? (s (set 1 2 3 4))null)

;more za zaporedje
(fri(any? (s (set 1 2 3 4 (true) (false))))null) ;check
(fri(any? (s (set (false) (false)))null) ;check

;delam funkcije
(fri(fun "sestevanje" (add (zz 1) (zz 2)) "a"))

(fri(all? (.. (true) (true)))null)
(fri(all? (.. (false) (false)))null)
(fri(any? (.. (false) (.. (false) (zz 1))))null)
(fri(any? (.. (false) (.. (false) (false))))null)
(fri(all? (s (set(true) (false)))))(fri(any? (s (set(false))))null)
(fri(any? (s (set(false))))null)
(fri(any? (s (set(false) (true))))null)

(fri (add (.. (zz 2) (empty))(.. (zz 22) (empty))) null)
(fri (vars (list "a" "b") (list (zz 11)(add(qq (zz 1)(zz 2))(zz 2))) (valof "b")) null)


;==========pluse se testi==========
(equal? (fri (qq (zz 2) (zz 8)) null) (qq (zz 1) (zz 4)))
(equal? (fri (.. (zz 42) (.. (qq (zz 3) (zz 30)) (empty))) null) (.. (zz 42) (.. (qq (zz 1) (zz 10)) (empty))))
(equal? (fri (true) null) (true))
(equal? (fri (false) null) (false))
(equal? (fri (if-then-else (add (false) (false)) (add (zz 5) (qq (zz 3) (zz 5))) (zz 28)) null) (zz 28))
(equal? (fri (is-qq? (add (zz 5) (qq (zz 2) (zz 3)))) null) (true))
(equal? (fri (add (.. (zz 3) (.. (zz 4) (.. (zz 5) (empty)))) (.. (zz 6) (.. (zz 7) (empty)))) null) (.. (zz 3) (.. (zz 4) (.. (zz 5) (.. (zz 6) (.. (zz 7) (empty)))))))
(equal? (fri (mul (s (set (true) (false) (zz 3))) (s (set (zz 4) (zz 4)))) null) (s (set (.. (true) (zz 4)) (.. (false) (zz 4)) (.. (zz 3) (zz 4)))))
(equal? (fri (rounding (qq (zz 7) (zz 3))) null) (zz 2))
(equal? (fri (all? (s (set (zz 1) (zz 2) (zz 3)))) null) (true))
(equal? (fri (qq (zz 9) (zz 18)) null) (qq (zz 1) (zz 2)))
(equal? (fri (is-qq? (add (qq (zz 3) (zz 2)) (qq (zz 3) (zz 1)))) null) (true))
(equal? (fri (is-bool? (true)) null) (true))
(equal? (fri (is-empty? (right (.. (zz 2) (empty)))) null) (true))
(equal? (fri (is-seq? (empty)) null) (true))
(equal? (fri (leq? (.. (true) (.. (false) (empty))) (.. (true) (empty))) null) (false))
(equal? (fri (rounding (zz 1)) null) (zz 1))
(equal? (fri (rounding (qq (zz -3) (zz 2))) null) (zz -2))
(equal? (fri (all? (empty)) null) (true))
(equal? (fri (any? (empty)) null) (false))
(equal? (fri (vars "a" (zz 1) (vars "a" (zz 2) (add (valof "a") (valof "a")))) null) (zz 4))))
(equal? (fri (numerator (qq (zz 4) (zz 6))) null) (zz 2))
(equal? (fri (denominator (qq (zz 4) (zz 6))) null) (zz 3))