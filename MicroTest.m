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

(* Is General::stop turned off? *)
generalStopQ[] := Head[General::stop] =!= $Off

SetAttributes[catchMessages, HoldAllComplete]
catchMessages[expr_] :=
    Module[{countSoFar},
      countSoFar = Length[$MessageList];
      With[{result = expr},
        {HoldComplete[result], Drop[$MessageList, countSoFar]}
      ]
    ]

heldToString[HoldComplete[expr_]] := ToString[Unevaluated[expr], InputForm]

print[style___][args___] := Print @@ (Style[#,style]& /@ {args})

logPrint = print[Red];

logTestResult[asc_] :=
    Module[{},
      If[asc@"Passed" && asc@"MsgPassed", Return[Null]];
      If[asc@"Section" =!= None, logPrint[Style["Section ", Bold], Style[asc@"Section", Bold]]];
      If[Not@asc@"Passed",
        logPrint["Failed: ", asc@"Test", "\nExpected: ", asc@"Expected", "\nGot: ", asc@"Got"];
      ];
      If[Not@asc@"MsgPassed",
        logPrint["Messages failed: ", asc@"Test", "\nExpected: ", asc@"MsgExpected", "\nGot: ", asc@"MsgGot"];
      ];
    ]

(* TextGrid[] is not available before 10.3 and does not work in command line mode even in later versions. *)
textGrid[arg_, opts___] := If[$Notebooks, Text, Identity]@Grid[arg, opts, Alignment -> Left]

SetAttributes[MTRun, HoldAllComplete]
MTRun[expr_] :=
    Block[
      {
        MT, MTSection, $section = None, gs = generalStopQ[], result,
        totalCount = 0, passCount = 0, failCount = 0, msgFailCount = 0 (* test counters *)
      },
      Off[General::stop];
      SetAttributes[MT, HoldAllComplete];
      MTSection[name_] := ($section = name; print[Darker@Blue][name]);
      Options[MT] = {SameTest -> SameQ};
      MT[test_, expected_, messages : {___} : {}, OptionsPattern[]] :=
          Module[{res, msgres, msgexp, pass, msgpass},
            (* TODO: Should have proper test error notification *)
            If[Not@MatchQ[HoldComplete[messages], HoldComplete[{___MessageName}]],
              print[Darker@Green][StringForm["`` is not a valid list of message names. Skipping test.", HoldForm[messages]]];
              Return[Null]
            ];
            Quiet[
              {res, msgres} = catchMessages[test],
              messages
            ];
            msgexp = Map[HoldForm, Unevaluated[messages]];
            pass = TrueQ@With[{e = expected}, OptionValue[SameTest][First[res], e]];
            msgpass = Union[msgres] === Union[msgexp];

            (* Count tests *)
            If[pass && msgpass, passCount++, failCount++];
            If[! msgpass, msgFailCount++];
            totalCount++;

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

      (* Print final report: *)
      Print@textGrid[{
        {"Tests run:", totalCount},
        {"Passed:", passCount},
        {"Failed: ", Style[failCount, If[failCount > 0, Red, Unevaluated@Sequence[]]]},
        {"Messages: ", Style[msgFailCount, If[msgFailCount > 0, Darker@Yellow, Unevaluated@Sequence[]]]}
      }, Frame -> True];

      result
    ]

End[] (* `Private` *)

EndPackage[]