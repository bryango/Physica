(* ::Package:: *)

(* ::Title:: *)
(*MathUtils*)


(* ::Subtitle:: *)
(*For an up-to-date version, go to: https://github.com/bryango/Physica*)


(* ::Section:: *)
(*Options*)


Quiet[
    SetOptions[Solve, Assumptions -> True];
    SetOptions[Language`ExtendedDefinition, ExcludedContexts -> {}],
    {SetOptions::optnf}
];


(* ::Section:: *)
(* Mathematics *)


addAssumptions = Catch[
    If[Simplify[And @@ {##}], Throw[$Assumptions]];
    $Assumptions = Simplify[
        $Assumptions && And @@ {##},
        Assumptions -> True
    ]
] &;


ClearAll[redify]
MakeBoxes[redify[x_], StandardForm] ^:= ToBoxes[Style[x, Red]]
unred = ReplaceAll[redify -> (# &)];


ClearAll[unNest]
unNest[f_ : List] := ReplaceRepeated[f@f@x___ :> f@x]
unpack := ReplaceAll[ {x_} :> x ] @* unNest[List]


ClearAll[actOnNumeratorDenominator, depressMinus]
actOnNumeratorDenominator[func_] := 
 Apply[#1/#2 &]@*func@*NumeratorDenominator
depressMinus := (-# &)@*actOnNumeratorDenominator[Apply[{-#1, #2} &]]


SetAttributes[holdItems, HoldAll]
holdItems[list_] := ReleaseHold[
    MapAt[Hold, Hold[list], {All, All}]
];


collectCoefficient[expr_, coefficient_, showForm_: Null] :=
Module[{form = showForm, remaining},
    If[MatrixQ[expr] && form == Null, form = MatrixForm];
    If[form == Null, form = HoldForm@*Evaluate@*Simplify];
    HoldForm[ HoldForm[coefficient] reduced ] /. {
        reduced -> form[expr/coefficient]
    }
];


(* ::Section:: *)
(* Context Management *)


wipeAll[context_: "Global`"] := (Quiet[
    # @@ Names[context ~~ "*" ~~ "`*" ...],
    {Remove::rmnsm, Remove::relex}
] & /@ { Unprotect, Remove };);


prependContext[context_] := If[ ! First @ $ContextPath == context,
    PrependTo[$ContextPath, context];
];



(* ::Section:: *)
(* Import & Export *)


saveScript[] := FrontEndExecute[
    FrontEndToken[EvaluationNotebook[], "Save", {
        NotebookDirectory[] <> FileBaseName[NotebookFileName[]] <> ".wl",
        "Script"
    }]
];


toLaTeX[expr_] := expr \
    // TraditionalForm \
    // ExportString[#, "TeXFragment"] & \
    // StringTrim[#] & \
    // StringDelete[RegularExpression["^\\\\\[|\\\\\]$"]];


(* ::Section:: *)
(* Inspections *)


"## show exec time & pass output";
SetAttributes[timeExec, {HoldAll, SequenceHold}];
timeExec[operations__] := (
   Print[Now];
   (Print[#1]; #2) & @@ Timing[operations]
);


printPrevious[printCmd_: Identity] := Print[% // printCmd];


"## inspect variable";
SetAttributes[printName, HoldAll];
printName[var_] := Print[{
    SymbolName[Unevaluated[var]], var
}];
