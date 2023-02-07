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
    If[Simplify[And @@ ##], Throw[$Assumptions]];
    $Assumptions = Simplify[
        $Assumptions && And @@ ##,
        Assumptions -> True
    ]
] &;


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


"## check if expression is function";
"## ... reference: <https://stackoverflow.com/a/3748658/10829731>";
functionQ[
    _Function | _InterpolatingFunction | _CompiledFunction
] = True;
functionQ[f_Symbol] := Or[
    DownValues[f] =!= {},
    MemberQ[Attributes[f], NumericFunction]
];
functionQ[_] = False;

"## simplify functional expressions";
fSimplify[ expr_Function | expr_Composition ] := Module[{
    func = expr
},
    While[Head[func] =!= Function,
        If[Head[func] =!= Composition,
            Message[fSimplify::notfunc, func];
            Abort[];
        ];
        func = func // Last;
    ];
    Function[Evaluate[Simplify[
        (expr) @@ (Slot /@ Range[Length[func[[1]]]])
    ]]]
];
fSimplify::notfunc = "`1` is not a Function or a Composition of Functions."


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
