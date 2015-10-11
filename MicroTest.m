(* Mathematica Package *)
(* Created by Mathematica plugin for IntelliJ IDEA *)

(* :Title: MicroTest *)
(* :Context: MicroTest` *)
(* :Author: szhorvat *)
(* :Date: 2015-09-02 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2015 Szabolcs Horvát *)
(* :Keywords: unit testing *)
(* :Discussion: *)

BeginPackage["MicroTest`"]

MT::usage = "MT[expression, expectedOutput, {expectedMessages}]";
MTRun::usage = "MTRun[expr]";
MTSection::usage = "MTSection[name]";

Begin["`Private`"]

SetAttributes[MT, HoldAllComplete];

generalStopQ[] := Head[General::stop] =!= $Off

SetAttributes[catchMessages, HoldAllComplete]
catchMessages[expr_] :=
    Module[{sofar},
      sofar = Length[$MessageList];
      With[{result = expr},
        {HoldComplete[result], Drop[$MessageList, sofar]}
      ]
    ]

heldToString[HoldComplete[expr_]] := ToString@Unevaluated[expr]

print[style___][args___] := Print @@ (Style[#,style]& /@ {args})

logPrint = print[Red];

logTestResult[asc_] :=
    Module[{},
      If[asc@"Passed" && asc@"MsgPassed", Return[Null]];
      If[asc@"Section" =!= None, logPrint["Section ", asc@"Section"]];
      If[Not@asc@"Passed",
        logPrint["Failed: ", asc@"Test", "\nExpected: ", asc@"Expected", "\nGot: ", asc@"Got"];
      ];
      If[Not@asc@"MsgPassed",
        logPrint["Messages failed: ", asc@"Test", "\nExpected: ", asc@"MsgExpected", "\nGot: ", asc@"MsgGot"];
      ];
    ]


SetAttributes[MTRun, HoldAllComplete]
MTRun[expr_] :=
    Block[{MT, MTSection, $section = None, gs = generalStopQ[], result},
      Off[General::stop];
      SetAttributes[MT, HoldAllComplete];
      MTSection[name_] := ($section = name; print[Darker@Blue][name]);
      MT[test_, expected_, messages_ : {}] :=
          Module[{res, msgres, msgexp, pass, msgpass},
            Quiet[
              {res, msgres} = catchMessages[test],
              messages
            ];
            msgexp = Map[HoldForm, Unevaluated[messages]];
            pass = With[{e = expected}, res === HoldComplete[e]];
            msgpass = Complement[msgres, msgexp] === {};
            logTestResult[
              <|
                "Passed" -> pass, "MsgPassed" -> msgpass,
                "Test" -> heldToString@HoldComplete[test],
                "Expected" -> heldToString@HoldComplete[expected], "Got" -> heldToString[res],
                "MsgExpected" -> DeleteDuplicates[msgexp], "MsgGot" -> DeleteDuplicates[msgres],
                "Section" -> $section
              |>
            ]
          ];
      MT[args___] := Print["Invalid MT arguments in ", $section, ": ", HoldForm[{args}]];
      result = expr;
      If[gs, On[General::stop]];
      result
    ]

End[] (* `Private` *)

EndPackage[]