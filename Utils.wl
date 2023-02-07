#!/usr/bin/env wolframscript
(* ::Package:: *)
(* For an up-to-date version, go to: https://github.com/bryango/Physica *)


<< "Physica/MathUtils.wl"

BeginPackage["Utils`"]
`Private`packageName = Context[];

Begin["`Private`"] (* Un-scoped variables defaults to `Private` *)


(* ::Section:: *)
(* Context Management *)

"## clear old definitions, from MathUtils`";
wipeAll["Utils`"];


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


"### show public information";
If[ ! inspectUtils,
    "DoNothing";
    , ReleaseHold[#], ReleaseHold[#]
] & @ Hold[
    Print[Style[NotebookFileName[], Bold, Larger]];
    Information["Utils`*"]
]
