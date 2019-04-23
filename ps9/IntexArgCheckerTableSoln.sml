(* Assume this will be loaded elsewhere *)
(* use "../intex/Intex.sml"; *)

structure IntexArgCheckerTableSoln = struct

exception UnhandledArgument

open Intex

val negativeIndex = Intex(3, BinApp(Mul,
				    BinApp(Add, Arg 1, Arg ~2),
				    Arg 3))

val tooLargeIndex = Intex(3, BinApp(Mul,
				    BinApp(Add, Arg 1, Arg 2),
				    BinApp(Sub, Arg 4, Arg 3)))

val argCheckerSolutionTable = 
    [(sqr, true), 
     (avg, true), 
     (f2c, true),
     (divRem, true),
     (negativeIndex, false),
     (tooLargeIndex, false)]

val testPrograms = map (fn (input,output) => input) argCheckerSolutionTable

fun argChecker pgm = 
    case List.find (fn (p, _) => p = pgm) argCheckerSolutionTable of
	(SOME (_, result)) => result
      | NONE => raise UnhandledArgument

val checkBottomUp = argChecker
val checkTopDown = argChecker

end
