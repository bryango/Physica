#!/usr/bin/env wolframscript
(* ::Package:: *)
(* For an up-to-date version, go to:
    https://github.com/bryango/Physica
*)

BeginPackage["Utils`"]
`Private`packageName = Context[];

Begin["`Private`"] (* Un-scoped variables defaults to `Private` *)


(* ::Section:: *)
(* Context Management *)

Global`wipeAll[context_: "Global`"] := (Quiet[
    # @@ Names[context ~~ "*" ~~ "`*" ...],
    {Remove::rmnsm, Remove::relex}
] & /@ { Unprotect, Remove };);

"## clear old definitions";
Global`wipeAll["Utils`"];

prependContext[context_] := If[ ! First @ $ContextPath == context,
    PrependTo[$ContextPath, context];
];


(* ::Section:: *)
(* Auto Collapse *)

"## Reference: <https://mathematica.stackexchange.com/a/683/65246>";
Utils`autoCollapse[] := Module[{
        selectionCache, nb = EvaluationNotebook[]
    },
    If[ $FrontEnd =!= $Failed,
        SelectionMove[nb, Previous, Cell, 1];
        selectionCache = SelectedCells[] // Last;
        SelectionMove[nb, All, GeneratedCell];
        FrontEndTokenExecute["SelectionCloseUnselectedCells"];
        SelectionMove[selectionCache, After, Cell];
    ]
];

SetAttributes[Utils`hideShow, HoldFirst];
Utils`hideShow[x_, styles_: {}] := (
    Print[
        Style[x, Sequence @@ styles]
    ];
    autoCollapse[];
);

SetAttributes[Utils`hideInfo, HoldFirst];
Utils`hideInfo[info_, styles_: {}] := hideShow[
    "(* " <> ToString[info] <> " *)",
    {Bold}~Join~styles
];


(* ::Section:: *)
(* Misc tools *)


SetAttributes[Utils`holdItems, HoldAll]
Utils`holdItems[list_] := ReleaseHold[
    MapAt[HoldForm, Hold[list], {All, All}]
];

addAssumptions = Catch[
    If[Simplify[And @@ ##], Throw[$Assumptions]];
    $Assumptions = Simplify[
        $Assumptions && And @@ ##,
        Assumptions -> True
    ]
] &;

Utils`collectCoefficient[expr_, coefficient_, showForm_: Null] :=
Module[{form = showForm, remaining},
    If[MatrixQ[expr] && form == Null, form = MatrixForm];
    If[form == Null, form = HoldForm@*Evaluate@*Simplify];
    HoldForm[ HoldForm[coefficient] reduced ] /. {
        reduced -> form[expr/coefficient]
    }
];


(* ::Section:: *)
(* Plot Utils *)

Utils`colorPalette[theme_] := (
    ("DefaultPlotStyle" /. (
        Method /. Charting`ResolvePlotTheme[theme, ListPlot]
    )) /. Directive[x_, __] :> x
);

{Utils`colors, Utils`colorsDefault} = colorPalette /@ {
    "Scientific", "Default"
};

Utils`getPlotPoints[plot_] := Cases[
    plot, Line[pts_] :> Line[pts], Infinity
] // Flatten // Cases[
    #, Line[pts_] :> Sequence @@ pts, Infinity
] &;

SetAttributes[Utils`exportPlot, HoldAll];
Utils`exportPlot[plot_, dir_: "plots/"] := Export[
    dir <> ToString[HoldForm[plot]] <> ".pdf",
    ReleaseHold[plot]
];


(* ::Section:: *)
(* Import & Export *)

Utils`importNotebook[path_, open_: False] := Module[{
        nb, nbPath, context, action
    },
    context = Context[];
    If[ (nbPath = FindFile[path]) // FailureQ,
        Print[ToString[path] <> " - Not Found!"];
        Return[];
    ];
    nb = NotebookOpen[
        nbPath,
        CellContext -> context,
        Visible -> None
    ];
    NotebookEvaluate[nb];
    NotebookClose[nb];

    action := NotebookOpen[nbPath];
    If[open, action];
    Button[path, action]
];

"## generate gif from Manipulate[]";
Utils`manipulateGif[manipulate_, name_String, step_Integer] := Export[
    name <> ".gif",
    Import[
        Export[name <> ".avi", manipulate]
        , "ImageList"
    ][[1 ;; -1 ;; step]]
];

Utils`saveScript[] := FrontEndExecute[
    FrontEndToken[EvaluationNotebook[], "Save", {
        NotebookDirectory[] <> FileBaseName[NotebookFileName[]] <> ".wl",
        "Script"
    }]
];

"## generate LaTeX math expressions";
Utils`toLaTeX[expr_] := expr \
    // TraditionalForm \
    // ExportString[#, "TeXFragment"] & \
    // StringTrim[#] & \
    // StringDelete[RegularExpression["^\\\\\[|\\\\\]$"]];


(* ::Section:: *)
(* Mathematics *)

"## check if expression is function";
"## ... reference: <https://stackoverflow.com/a/3748658/10829731>";
Utils`functionQ[
    _Function | _InterpolatingFunction | _CompiledFunction
] = True;
functionQ[f_Symbol] := Or[
    DownValues[f] =!= {},
    MemberQ[Attributes[f], NumericFunction]
];
functionQ[_] = False;

"## simplify functional expressions";
Utils`fSimplify[ expr_Function | expr_Composition ] := Module[{
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
(* Inspections *)

"## show exec time & pass output";
SetAttributes[Utils`timeExec, {HoldAll, SequenceHold}];
Utils`timeExec[operations__] := (
   Print[Now];
   (Print[#1]; #2) & @@ Timing[operations]
);

Utils`printPrevious[printCmd_: Identity] := Print[% // printCmd];

"## inspect variable";
SetAttributes[Utils`printName, HoldAll];
Utils`printName[var_] := Print[{
    SymbolName[Unevaluated[var]], var
}];


(* ::Section:: *)
(* Ending *)

End[] (* End `Private` *)
EndPackage[]

"### reasonable options";
SetOptions[Language`ExtendedDefinition, ExcludedContexts -> {}]
Quiet[
    SetOptions[Solve, Assumptions -> True];
    SetOptions[Language`ExtendedDefinition, ExcludedContexts -> {}],
    {SetOptions::optnf}
];

"### show public information";
If[ ! inspectUtils,
    "DoNothing";
    , ReleaseHold[#], ReleaseHold[#]
] & @ Hold[
    Print[Style[NotebookFileName[], Bold, Larger]];
    Information["Utils`*"]
]
