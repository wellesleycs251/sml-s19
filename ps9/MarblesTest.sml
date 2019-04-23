use "../utils/Tester.sml";
use "../utils/Show.sml";
open Show; (* toString functions *)
use "MarblesTableSoln.sml";
use "marbles.sml";

signature MARBLES_TEST = sig
val testMarbles: unit -> bool * int * int
end

structure MarblesTest :> MARBLES_TEST = 

struct

fun curriedPair x y = (x, y) (* curried pairing operator *)

fun range lo hi = (* list all ints from lo up to (but not including) hi *)
  List.tabulate(hi - lo, fn i => i + lo)

fun uncurry2 f = fn (a, b) => f a b

fun cartesianProduct xs ys =
    foldr (fn (x, subres) => (List.map (curriedPair x) ys) @ subres)
          [] 
          xs

fun testMarbles() =
    Tester.testFn ("marbles", 
		   pair(int, int), 
		   list (list int), 
		   uncurry2 marbles, 
		   uncurry2 MarblesTableSoln.marbles, 
		   cartesianProduct (range 0 6) (range 1 6)
		  )

end

val testResults = MarblesTest.testMarbles()


