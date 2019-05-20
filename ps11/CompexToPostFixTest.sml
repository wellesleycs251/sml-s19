use "CompexToPostFix.sml"; (* rename this to be your version! *)
use "../utils/Tester.sml";
use "../utils/Show.sml";
open Show; (* toString functions *)
use "CompexEnvInterpTableSoln.sml";

signature COMPEX_TO_POSTFIX_TEST = sig end

structure CompexToPostFixTest :> COMPEX_TO_POSTFIX_TEST = struct

(* Compile and print each program only once *)
fun compileAndPrint filename = 
    let val postFixPgm = CompexToPostFix.compexToPostFix 
			     (Compex.sexpToPgm (Compex.fileToSexp filename))
	val _ = print ("\nResult of compiling " ^ filename ^ ":")
	val _ = print ("\n" ^ (PostFix.pgmToString postFixPgm) ^ "\n")
    in (filename, postFixPgm)
    end
	
val compiledPgms = map compileAndPrint 
		       ["pgm1.cpx", "pgm2.cpx", "pgm3.cpx", 
			"pgm4.cpx", "pgm5.cpx", "pgm6.cpx"]

fun getCompiledPgm filename = 
    case (List.find (fn (fname, pgm) => fname = filename)
		    compiledPgms) of
	SOME(fname, pgm) => pgm
      | _ => raise Fail ("getCompiledPgm: can't find program for " ^ filename)
	     

fun testCompexToPostFixFile(filename, args) = 
    PostFix.testRun (getCompiledPgm filename) args
    handle CompexEnvInterp.SyntaxError s => ("SyntaxError: " ^ s)
	 | Fail s =>("FailError: " ^ s)
	 | other => ("Error: " ^ (exnName other)
		     ^ " -- " ^ (exnMessage other))

fun testCompexToPostFix() = 
    Tester.testFn ("runCompexToPostFixFile", 
		   pair(string, list int), 
		   string, 
		   testCompexToPostFixFile, 
		   CompexEnvInterpTableSoln.testRunFile, 
		   [("pgm1.cpx", [2,3]),
		    ("pgm1.cpx", [~3,4]),
		    ("pgm1.cpx", [4,~5]),
		    ("pgm1.cpx", [~5,~6]),

		    ("pgm1.cpx", [~6,0]),
		    ("pgm1.cpx", [0,7]),
		    ("pgm2.cpx", [6,2]),
		    ("pgm2.cpx", [3,3]),
		    ("pgm2.cpx", [2,6]), 

		    ("pgm3.cpx", [1,2,3]), 
		    ("pgm3.cpx", [1,3,2]),
		    ("pgm3.cpx", [2,1,3]), 
		    ("pgm3.cpx", [2,3,1]),
		    ("pgm3.cpx", [3,1,2]),
		    ("pgm3.cpx", [3,2,1]),

		    ("pgm4.cpx", [3,2,1]),
		    ("pgm4.cpx", [10,7,3]),
		    ("pgm4.cpx", [17,17,17]),
		    ("pgm4.cpx", [~5,~5,~5]),
		    ("pgm4.cpx", [2,3,4]),
		    ("pgm4.cpx", [~2,3,4]),
		    ("pgm4.cpx", [~5,~4,3]),
		    ("pgm4.cpx", [~5,~4,~3]),

		    ("pgm5.cpx", [10,3,9]), 
		    ("pgm5.cpx", [10,5,9]), 
		    ("pgm5.cpx", [10,7,9]), 
		    ("pgm5.cpx", [~3,3,9]), 
		    ("pgm5.cpx", [~5,3,9]), 
		    ("pgm5.cpx", [10,3,3]), 
		    ("pgm5.cpx", [10,5,5]), 
		    ("pgm5.cpx", [10,7,7]), 
		    ("pgm5.cpx", [3,3,3]), 
		    ("pgm5.cpx", [~3,3,3]), 
		    ("pgm5.cpx", [~7,3,3]), 
		    ("pgm5.cpx", [10,3,~5]), 
		    ("pgm5.cpx", [10,5,~5]), 
		    ("pgm5.cpx", [10,7,~5]), 
		    ("pgm5.cpx", [~3,3,~5]), 
		    ("pgm5.cpx", [~7,3,~5]), 

		    ("pgm6.cpx", [2,3,4,5,6,7,8,9]), 
		    ("pgm6.cpx", [2,3,4,5,6,7,8,~8]), 
		    ("pgm6.cpx", [2,3,4,5,6,7,8,~21]),
		    ("pgm6.cpx", [2,3,4,5,6,~6,8,9]), 
		    ("pgm6.cpx", [2,3,4,5,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,3,4,5,6,~6,8,~21]),
		    ("pgm6.cpx", [2,3,4,5,6,~18,8,9]), 
		    ("pgm6.cpx", [2,3,4,5,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,3,4,5,6,~18,8,~21]),
		    ("pgm6.cpx", [2,3,4,~4,6,7,8,9]), 
		    ("pgm6.cpx", [2,3,4,~4,6,7,8,~8]), 
		    ("pgm6.cpx", [2,3,4,~4,6,7,8,~21]),
		    ("pgm6.cpx", [2,3,4,~4,6,~6,8,9]), 
		    ("pgm6.cpx", [2,3,4,~4,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,3,4,~4,6,~6,8,~21]),
		    ("pgm6.cpx", [2,3,4,~4,6,~18,8,9]), 
		    ("pgm6.cpx", [2,3,4,~4,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,3,4,~4,6,~18,8,~21]),
		    ("pgm6.cpx", [2,3,4,~15,6,7,8,9]), 
		    ("pgm6.cpx", [2,3,4,~15,6,7,8,~8]), 
		    ("pgm6.cpx", [2,3,4,~15,6,7,8,~21]),
		    ("pgm6.cpx", [2,3,4,~15,6,~6,8,9]), 
		    ("pgm6.cpx", [2,3,4,~15,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,3,4,~15,6,~6,8,~21]),
		    ("pgm6.cpx", [2,3,4,~15,6,~18,8,9]), 
		    ("pgm6.cpx", [2,3,4,~15,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,3,4,~15,6,~18,8,~21]),
		    ("pgm6.cpx", [2,~2,4,5,6,7,8,9]), 
		    ("pgm6.cpx", [2,~2,4,5,6,7,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,5,6,7,8,~21]),
		    ("pgm6.cpx", [2,~2,4,5,6,~6,8,9]), 
		    ("pgm6.cpx", [2,~2,4,5,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,5,6,~6,8,~21]),
		    ("pgm6.cpx", [2,~2,4,5,6,~18,8,9]), 
		    ("pgm6.cpx", [2,~2,4,5,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,5,6,~18,8,~21]),
		    ("pgm6.cpx", [2,~2,4,~4,6,7,8,9]), 
		    ("pgm6.cpx", [2,~2,4,~4,6,7,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,~4,6,7,8,~21]),
		    ("pgm6.cpx", [2,~2,4,~4,6,~6,8,9]), 
		    ("pgm6.cpx", [2,~2,4,~4,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,~4,6,~6,8,~21]),
		    ("pgm6.cpx", [2,~2,4,~4,6,~18,8,9]), 
		    ("pgm6.cpx", [2,~2,4,~4,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,~4,6,~18,8,~21]),
		    ("pgm6.cpx", [2,~2,4,~15,6,7,8,9]), 
		    ("pgm6.cpx", [2,~2,4,~15,6,7,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,~15,6,7,8,~21]),
		    ("pgm6.cpx", [2,~2,4,~15,6,~6,8,9]), 
		    ("pgm6.cpx", [2,~2,4,~15,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,~15,6,~6,8,~21]),
		    ("pgm6.cpx", [2,~2,4,~15,6,~18,8,9]), 
		    ("pgm6.cpx", [2,~2,4,~15,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,~2,4,~15,6,~18,8,~21]),
		    ("pgm6.cpx", [2,~12,4,5,6,7,8,9]), 
		    ("pgm6.cpx", [2,~12,4,5,6,7,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,5,6,7,8,~21]),
		    ("pgm6.cpx", [2,~12,4,5,6,~6,8,9]), 
		    ("pgm6.cpx", [2,~12,4,5,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,5,6,~6,8,~21]),
		    ("pgm6.cpx", [2,~12,4,5,6,~18,8,9]), 
		    ("pgm6.cpx", [2,~12,4,5,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,5,6,~18,8,~21]),
		    ("pgm6.cpx", [2,~12,4,~4,6,7,8,9]), 
		    ("pgm6.cpx", [2,~12,4,~4,6,7,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,~4,6,7,8,~21]),
		    ("pgm6.cpx", [2,~12,4,~4,6,~6,8,9]), 
		    ("pgm6.cpx", [2,~12,4,~4,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,~4,6,~6,8,~21]),
		    ("pgm6.cpx", [2,~12,4,~4,6,~18,8,9]), 
		    ("pgm6.cpx", [2,~12,4,~4,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,~4,6,~18,8,~21]),
		    ("pgm6.cpx", [2,~12,4,~15,6,7,8,9]), 
		    ("pgm6.cpx", [2,~12,4,~15,6,7,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,~15,6,7,8,~21]),
		    ("pgm6.cpx", [2,~12,4,~15,6,~6,8,9]), 
		    ("pgm6.cpx", [2,~12,4,~15,6,~6,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,~15,6,~6,8,~21]),
		    ("pgm6.cpx", [2,~12,4,~15,6,~18,8,9]), 
		    ("pgm6.cpx", [2,~12,4,~15,6,~18,8,~8]), 
		    ("pgm6.cpx", [2,~12,4,~15,6,~18,8,~21])
		   ])

val testResults = testCompexToPostFix()

end



