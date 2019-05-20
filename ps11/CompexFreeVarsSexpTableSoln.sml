structure CompexFreeVarsSexpTableSoln = struct

exception SolutionOutOfRange

val compexFreeVarsSexpSolutionTable = [
    ("(comp x 1 -2 0 2)", "{}"),
    ("(comp x (- a b) (- x (+ c d)) (/ (- e f) x) (/ x (+ g h)))", 
     "{a,b,c,d,e,f,g,h}"), 
    ("(comp x (/ x (- a b)) (- x (+ c d)) (/ (- e f) x) (/ x (- g h)))", 
     "{a,b,c,d,e,f,g,h,x}"),
    ("(comp p (- a b)\ 
     \     (comp q (/ p c) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
     \     (comp r (/ p g) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
     \     (comp s (/ p k) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
     "{a,b,c,d,e,f,g,h,i,j,k,l,m,n}"), 
    ("(comp p (- a p)\ 
     \     (comp q (/ p c) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
     \     (comp r (/ p g) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
     \     (comp s (/ p k) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
     "{a,c,d,e,f,g,h,i,j,k,l,m,n,p}"), 
    ("(comp p (- a b)\ 
     \     (comp q (/ p q) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
     \     (comp r (/ p g) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
     \     (comp s (/ p k) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
     "{a,b,d,e,f,g,h,i,j,k,l,m,n,q}"), 
    ("(comp p (- a b)\ 
     \     (comp q (/ p c) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
     \     (comp r (/ p r) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
     \     (comp s (/ p k) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
     "{a,b,c,d,e,f,h,i,j,k,l,m,n,r}"), 
    ("(comp p (- a b)\ 
     \     (comp q (/ p c) (/ p (- q d)) (/ p (+ q e)) (/ p (/ q f)))\
     \     (comp r (/ p g) (/ p (- h r)) (/ p (+ i r)) (/ p (/ j r)))\
     \     (comp s (/ p s) (/ p (- s l)) (/ p (+ s m)) (/ p (/ s n))))",
     "{a,b,c,d,e,f,g,h,i,j,l,m,n,s}")
]

fun compexFreeVarsSexp(sexp) = 
    case List.find (fn (sx, _) => sx = sexp)
		   compexFreeVarsSexpSolutionTable of 
	(SOME (_, output)) => output
      | NONE => raise SolutionOutOfRange

end
