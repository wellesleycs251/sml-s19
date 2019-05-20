use "Compex.sml"; (* rename this to be your version! *)
use "../utils/Tester.sml"; 
use "../utils/Show.sml"; 
open Show; (* toString functions *)
use "CompexFreeVarsSexpTableSoln.sml"; 

signature COMPEX_FREE_VARS_EXP_TEST = sig end

structure CompexFreeVarsExpTest :> COMPEX_FREE_VARS_EXP_TEST = struct

fun compexFreeVarsSexp(expAsSexpString) = 
    (* Return list of free vars (as strings) from Compex exp (as sexp string) *)
    Compex.S.toString(Compex.freeVarsExp(Compex.stringToExp expAsSexpString))

fun testCompexFreeVarsExp() = 
    Tester.testFn ("freeVarsExp", 
		   string, 
		   string, 
		   compexFreeVarsSexp, 
		   CompexFreeVarsSexpTableSoln.compexFreeVarsSexp,
		   ["(comp x 1 -2 0 2)", 
		    "(comp x (- a b) (- x (+ c d)) (/ (- e f) x) (/ x (+ g h)))", 
		    "(comp x (/ x (- a b)) (- x (+ c d)) (/ (- e f) x) (/ x (- g h)))",
		    "(comp p (- a b)\ 
                     \     (comp q (/ p c) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
                     \     (comp r (/ p g) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
                     \     (comp s (/ p k) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
		    "(comp p (- a p)\ 
                     \     (comp q (/ p c) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
                     \     (comp r (/ p g) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
                     \     (comp s (/ p k) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
		    "(comp p (- a b)\ 
                     \     (comp q (/ p q) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
                     \     (comp r (/ p g) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
                     \     (comp s (/ p k) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
		    "(comp p (- a b)\ 
                     \     (comp q (/ p c) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
                     \     (comp r (/ p r) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
                     \     (comp s (/ p k) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
		    "(comp p (- a b)\ 
                     \     (comp q (/ p c) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
                     \     (comp r (/ p g) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
                     \     (comp s (/ p s) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))"
		   ]
		  )

val testResults = testCompexFreeVarsExp()

end


