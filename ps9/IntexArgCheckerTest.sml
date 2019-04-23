use "../utils/Tester.sml";
use "../utils/Show.sml";

(* Assume this will be loaded by student code *)
(* use "../intex/Intex.sml"; *)
use "IntexArgChecker.sml";
use "IntexArgCheckerTableSoln.sml";

signature INTEX_ARG_CHECKER_TEST = sig
  (* Having an empty signature avoids display any module elements;
     only test results from print are shown. *)
end


structure IntexArgCheckerTest :> INTEX_ARG_CHECKER_TEST = struct

fun testCheckBottomUp() =
    Tester.testFn ("checkBottomUp", 
		   Intex.pgmToString, 
		   Show.bool, 
		   checkBottomUp, 
		   IntexArgCheckerTableSoln.checkBottomUp, 
		   IntexArgCheckerTableSoln.testPrograms
		  )

fun testCheckTopDown() =
    Tester.testFn ("checkTopDownUp", 
		   Intex.pgmToString, 
		   Show.bool, 
		   checkTopDown, 
		   IntexArgCheckerTableSoln.checkTopDown, 
		   IntexArgCheckerTableSoln.testPrograms
		  )

val bottomUpResults = testCheckBottomUp()
val topDownResults = testCheckTopDown()

end




