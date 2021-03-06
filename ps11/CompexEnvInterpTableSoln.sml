structure CompexEnvInterpTableSoln = struct

exception SolutionOutOfRange

(* solutions for tupled marbles function for inputs from (0,1) through (5,5) *)
val compexEnvInterpSolutionTable =

  [(("pgm1.cpx", [2,3]), "6"), 
   (("pgm1.cpx", [~3,4]), "12"), 
   (("pgm1.cpx", [4,~5]), "20"), 
   (("pgm1.cpx", [~5,~6]), "30"),
   (("pgm1.cpx", [~6,0]), "0"), 
   (("pgm1.cpx", [0,7]), "0"), 

   (("pgm2.cpx", [6,2]), "16"), 
   (("pgm2.cpx", [3,3]), "6"),
   (("pgm2.cpx", [2,6]), "~8"),

   (("pgm3.cpx", [1,2,3]), "3"),
   (("pgm3.cpx", [1,3,2]), "3"),
   (("pgm3.cpx", [2,1,3]), "3"),
   (("pgm3.cpx", [2,3,1]), "3"),
   (("pgm3.cpx", [3,1,2]), "3"),
   (("pgm3.cpx", [3,2,1]), "3"), 

   (("pgm4.cpx", [10,7,3]), "20"), 
   (("pgm4.cpx", [17,17,17]), "17"), 
   (("pgm4.cpx", [~5,~5,~5]), "~5"), 
   (("pgm4.cpx", [2,3,4]), "24"), 
   (("pgm4.cpx", [~2,3,4]), "~24"), 
   (("pgm4.cpx", [~5,~4,3]), "60"), 
   (("pgm4.cpx", [~5,~4,~3]), "~60"), 

   (* Need more tests to cover all cases *)
   (("pgm5.cpx", [10,3,9]), "35"), 
   (("pgm5.cpx", [10,5,9]), "19"), 
   (("pgm5.cpx", [10,7,9]), "~12"), 
   (("pgm5.cpx", [~3,3,9]), "~81"), 
   (("pgm5.cpx", [~5,3,9]), "~99"), 
   (("pgm5.cpx", [10,3,3]), "14"), 
   (("pgm5.cpx", [10,5,5]), "0"), 
   (("pgm5.cpx", [10,7,7]), "~21"), 
   (("pgm5.cpx", [3,3,3]), "~6"), 
   (("pgm5.cpx", [~3,3,3]), "~24"), 
   (("pgm5.cpx", [~7,3,3]), "~36"), 
   (("pgm5.cpx", [10,3,~5]), "21"), 
   (("pgm5.cpx", [10,5,~5]), "5"), 
   (("pgm5.cpx", [10,7,~5]), "~18"), 
   (("pgm5.cpx", [~3,3,~5]), "~30"), 
   (("pgm5.cpx", [~7,3,~5]), "~45"), 

   (("pgm4.cpx", [3,2,1]), "6"), 


   (("pgm6.cpx", [2,3,4,5,6,7,8,9]), "49"), 
   (("pgm6.cpx", [2,3,4,5,6,7,8,~8]), "49"), 
   (("pgm6.cpx", [2,3,4,5,6,7,8,~21]), "49"),
   (("pgm6.cpx", [2,3,4,5,6,~6,8,9]), "65"),
   (("pgm6.cpx", [2,3,4,5,6,~6,8,~8]), "65"),
   (("pgm6.cpx", [2,3,4,5,6,~6,8,~21]), "65"),
   (("pgm6.cpx", [2,3,4,5,6,~18,8,9]), "~526"),
   (("pgm6.cpx", [2,3,4,5,6,~18,8,~8]), "~526"),
   (("pgm6.cpx", [2,3,4,5,6,~18,8,~21]), "~526"),
   (("pgm6.cpx", [2,3,4,~4,6,7,8,9]), "~63"), 
   (("pgm6.cpx", [2,3,4,~4,6,7,8,~8]), "109"), 
   (("pgm6.cpx", [2,3,4,~4,6,7,8,~21]), "~43"),
   (("pgm6.cpx", [2,3,4,~4,6,~6,8,9]), "~63"),
   (("pgm6.cpx", [2,3,4,~4,6,~6,8,~8]), "109"),
   (("pgm6.cpx", [2,3,4,~4,6,~6,8,~21]), "~43"),
   (("pgm6.cpx", [2,3,4,~4,6,~18,8,9]), "~63"),
   (("pgm6.cpx", [2,3,4,~4,6,~18,8,~8]), "109"),
   (("pgm6.cpx", [2,3,4,~4,6,~18,8,~21]), "~43"),
   (("pgm6.cpx", [2,3,4,~15,6,7,8,9]), "~330"), 
   (("pgm6.cpx", [2,3,4,~15,6,7,8,~8]), "~330"), 
   (("pgm6.cpx", [2,3,4,~15,6,7,8,~21]), "~330"),
   (("pgm6.cpx", [2,3,4,~15,6,~6,8,9]), "~330"),
   (("pgm6.cpx", [2,3,4,~15,6,~6,8,~8]), "~330"),
   (("pgm6.cpx", [2,3,4,~15,6,~6,8,~21]), "~330"),
   (("pgm6.cpx", [2,3,4,~15,6,~18,8,9]), "~330"),
   (("pgm6.cpx", [2,3,4,~15,6,~18,8,~8]), "~330"),
   (("pgm6.cpx", [2,3,4,~15,6,~18,8,~21]), "~330"),
   (("pgm6.cpx", [2,~2,4,5,6,7,8,9]), "22"), 
   (("pgm6.cpx", [2,~2,4,5,6,7,8,~8]), "22"), 
   (("pgm6.cpx", [2,~2,4,5,6,7,8,~21]), "22"),
   (("pgm6.cpx", [2,~2,4,5,6,~6,8,9]), "70"),
   (("pgm6.cpx", [2,~2,4,5,6,~6,8,~8]), "70"),
   (("pgm6.cpx", [2,~2,4,5,6,~6,8,~21]), "70"),
   (("pgm6.cpx", [2,~2,4,5,6,~18,8,9]), "96"),
   (("pgm6.cpx", [2,~2,4,5,6,~18,8,~8]), "96"),
   (("pgm6.cpx", [2,~2,4,5,6,~18,8,~21]), "96"),
   (("pgm6.cpx", [2,~2,4,~4,6,7,8,9]), "4"), 
   (("pgm6.cpx", [2,~2,4,~4,6,7,8,~8]), "74"), 
   (("pgm6.cpx", [2,~2,4,~4,6,7,8,~21]), "0"),
   (("pgm6.cpx", [2,~2,4,~4,6,~6,8,9]), "36"),
   (("pgm6.cpx", [2,~2,4,~4,6,~6,8,~8]), "106"),
   (("pgm6.cpx", [2,~2,4,~4,6,~6,8,~21]), "32"),
   (("pgm6.cpx", [2,~2,4,~4,6,~18,8,9]), "5"),
   (("pgm6.cpx", [2,~2,4,~4,6,~18,8,~8]), "75"),
   (("pgm6.cpx", [2,~2,4,~4,6,~18,8,~21]), "1"),
   (("pgm6.cpx", [2,~2,4,~15,6,7,8,9]), "46"), 
   (("pgm6.cpx", [2,~2,4,~15,6,7,8,~8]), "46"), 
   (("pgm6.cpx", [2,~2,4,~15,6,7,8,~21]), "46"),
   (("pgm6.cpx", [2,~2,4,~15,6,~6,8,9]), "65"),
   (("pgm6.cpx", [2,~2,4,~15,6,~6,8,~8]), "65"),
   (("pgm6.cpx", [2,~2,4,~15,6,~6,8,~21]), "65"),
   (("pgm6.cpx", [2,~2,4,~15,6,~18,8,9]), "~564"),
   (("pgm6.cpx", [2,~2,4,~15,6,~18,8,~8]), "~564"),
   (("pgm6.cpx", [2,~2,4,~15,6,~18,8,~21]), "~564"),
   (("pgm6.cpx", [2,~12,4,5,6,7,8,9]), "~135"), 
   (("pgm6.cpx", [2,~12,4,5,6,7,8,~8]), "48"), 
   (("pgm6.cpx", [2,~12,4,5,6,7,8,~21]), "38"),
   (("pgm6.cpx", [2,~12,4,5,6,~6,8,9]), "~119"),
   (("pgm6.cpx", [2,~12,4,5,6,~6,8,~8]), "64"),
   (("pgm6.cpx", [2,~12,4,5,6,~6,8,~21]), "54"),
   (("pgm6.cpx", [2,~12,4,5,6,~18,8,9]), "~288"),
   (("pgm6.cpx", [2,~12,4,5,6,~18,8,~8]), "~105"),
   (("pgm6.cpx", [2,~12,4,5,6,~18,8,~21]), "~115"),
   (("pgm6.cpx", [2,~12,4,~4,6,7,8,9]), "~197"), 
   (("pgm6.cpx", [2,~12,4,~4,6,7,8,~8]), "100"), 
   (("pgm6.cpx", [2,~12,4,~4,6,7,8,~21]), "~20"),
   (("pgm6.cpx", [2,~12,4,~4,6,~6,8,9]), "~197"),
   (("pgm6.cpx", [2,~12,4,~4,6,~6,8,~8]), "100"),
   (("pgm6.cpx", [2,~12,4,~4,6,~6,8,~21]), "~20"),
   (("pgm6.cpx", [2,~12,4,~4,6,~18,8,9]), "~197"),
   (("pgm6.cpx", [2,~12,4,~4,6,~18,8,~8]), "100"),
   (("pgm6.cpx", [2,~12,4,~4,6,~18,8,~21]), "~20"),
   (("pgm6.cpx", [2,~12,4,~15,6,7,8,9]), "~181"), 
   (("pgm6.cpx", [2,~12,4,~15,6,7,8,~8]), "2"), 
   (("pgm6.cpx", [2,~12,4,~15,6,7,8,~21]), "~8"),
   (("pgm6.cpx", [2,~12,4,~15,6,~6,8,9]), "~181"),
   (("pgm6.cpx", [2,~12,4,~15,6,~6,8,~8]), "2"),
   (("pgm6.cpx", [2,~12,4,~15,6,~6,8,~21]), "~8"),
   (("pgm6.cpx", [2,~12,4,~15,6,~18,8,9]), "~181"),
   (("pgm6.cpx", [2,~12,4,~15,6,~18,8,~8]), "2"),
   (("pgm6.cpx", [2,~12,4,~15,6,~18,8,~21]), "~8")
  ]

fun testRunFile args =
    case List.find (fn (args', _) => args = args')
		   compexEnvInterpSolutionTable of
	(SOME (_, output)) => output
      | NONE => raise SolutionOutOfRange

end
