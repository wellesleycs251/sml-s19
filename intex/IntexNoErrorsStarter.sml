use "../sexp/Sexp.sml";

structure Intex =

struct

datatype pgm = Intex of int * exp
     and exp = Int of int
             | Arg of int
	     | BinApp of binop * exp * exp
     and binop = Add | Sub | Mul | Div | Rem

val sqr = Intex(1, BinApp(Mul, Arg 1, Arg 1))
val avg = Intex(2, BinApp(Div, BinApp(Add, Arg 1, Arg 2), Int 2))
(* val f2c = Flesh this Farenheit to Celsius converter! *)
val divRem = Intex(5, BinApp(Add,
			     BinApp(Mul,
				    BinApp(Div, Arg 1, Arg 2),
				    Arg 3),
			     BinApp(Rem, Arg 4, Arg 5)))

fun run (Intex(numargs, exp)) args =
  17

and eval (Int i) args = 17
  | eval (Arg index) args = 17
  | eval (BinApp(binop, exp1, exp2)) args =
      17

and binopToFun Add = op+
  | binopToFun Mul = op*
  | binopToFun Sub = op-
  | binopToFun Div = (fn(x,y) => x div y)
  | binopToFun Rem = (fn(x,y) => x mod y)

(* Parsing from S-Expressions *)

exception SyntaxError of string

fun sexpToPgm (Sexp.Seq[Sexp.Sym "intex", Sexp.Int n, body]) =
    Intex(n, sexpToExp body)
  | sexpToPgm sexp = raise (SyntaxError ("invalid Intex program: "
					 ^ (Sexp.sexpToString sexp)))

and sexpToExp (Sexp.Int i) = Int i
  | sexpToExp (Sexp.Seq[Sexp.Sym "$", Sexp.Int i]) = Arg i
  | sexpToExp (Sexp.Seq[Sexp.Sym p, rand1, rand2]) =
    BinApp(stringToPrimop p, sexpToExp rand1, sexpToExp rand2)
  | sexpToExp sexp =  raise (SyntaxError ("invalid Intex expression: "
					  ^ (Sexp.sexpToString sexp)))

and stringToPrimop "+" = Add
  | stringToPrimop "-" = Sub
  | stringToPrimop "*" = Mul
  | stringToPrimop "/" = Div
  | stringToPrimop "%" = Rem
  | stringToPrimop s = raise (SyntaxError ("invalid Intex primop: " ^ s))

and stringToExp s = sexpToExp (Sexp.stringToSexp s)
and stringToPgm s = sexpToPgm (Sexp.stringToSexp s)

(* Unparsing from S-Expressions *)

fun pgmToSexp (Intex(n,body)) =
  Sexp.Seq[Sexp.Sym "intex", Sexp.Int n, expToSexp body]

and expToSexp (Int i) = Sexp.Int i
  | expToSexp (Arg i) = Sexp.Seq[Sexp.Sym "$", Sexp.Int i]
  | expToSexp (BinApp (rator, rand1, rand2)) =
    Sexp.Seq[Sexp.Sym (primopToString rator),
	     expToSexp rand1, expToSexp rand2]

and primopToString Add = "+"
    | primopToString Sub = "-"
    | primopToString Mul = "*"
    | primopToString Div = "/"
    | primopToString Rem = "%"

and expToString s = Sexp.sexpToString (expToSexp s)
and pgmToString s = Sexp.sexpToString (pgmToSexp s)			      

end

(* Test cases *)

open Intex

val sqrTest = run sqr [5]
val avgTest = run avg [5,15]
(* val f2cTest = run f2c [86] *)

fun testRun pgm args =
  Int.toString (run pgm args) (* Convert to string so same type as error messages below *)
  handle exn => "Exception raised: " ^ (exnMessage exn)

exception SexpError of string * Sexp.sexp					      
(* testRun' takes sexpStrings instead *)
fun testRun' pgmSexpString argsSexpString =				     
    testRun (stringToPgm pgmSexpString)
	    (sexpStringToIntList argsSexpString)
    handle SexpError (msg, sexp) => ("SexpError: " ^ msg ^ " " ^ (Sexp.sexpToString sexp))
         | Sexp.IllFormedSexp msg => ("SexpError: Ill-formed sexp " ^ msg)
         | other => "Unknown exception: " ^ (exnMessage other)

and sexpStringToIntList str =
    let val sexp = Sexp.stringToSexp str
    in case sexp of
	   Sexp.Seq xs => List.map sexpToInt xs
	 | _  => raise SexpError("expected sexp sequence but got", sexp)
    end

and sexpToInt (Sexp.Int i) = i
  | sexpToInt sexp = raise SexpError("expected sexp int but got", sexp)

val avgTest2 = testRun' "(intex 2 (/ (+ ($ 1) ($ 2)) 2))" "(5 15)"

val f2cTest2 = List.map (testRun' "(intex 1 (/ (* (- ($ 1) 32) 5) 9))")
			["(-40)", "(0)", "(32)", "(98)", "(212)"] 

