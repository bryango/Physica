#!/usr/bin/env wolframscript
(* ::Package:: *)

BeginPackage["Utils`"]
`Private`packageName = Context[];

Begin["`Private`"] (* Un-scoped variables defaults to `Private` *)


"### Context Management";
Global`wipeAll[context_: "Global`"] := (Quiet[
    # @@ Names[context ~~ "*" ~~ "`*" ...],
    {ClearAll::wrsym, Remove::rmnsm, Remove::relex}
] & /@ { ClearAll, Remove };);

"# Clear old definitions";
Global`wipeAll["Utils`"];

Utils`prependContext[context_] := If[ ! MemberQ[$ContextPath, context],
    PrependTo[$ContextPath, context];
];


"### Auto Collapse Cell";
Utils`autoCollapse[] := (
    If[ $FrontEnd =!= $Failed,
        SelectionMove[EvaluationNotebook[], All, GeneratedCell];
        FrontEndTokenExecute["SelectionCloseUnselectedCells"]
    ]
);

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


"### Hold Items for Further Processing";
SetAttributes[Utils`holdItems, HoldAll]
Utils`holdItems[list_] := ReleaseHold[
    MapAt[HoldForm, Hold[list], {All, All}]
];


"### Plot Utils";
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

"### Export Plots";
SetAttributes[Utils`exportPlot, HoldAll];
Utils`exportPlot[plot_, dir_: "plots/"] := Export[
    dir <> ToString[HoldForm[plot]] <> ".pdf",
    ReleaseHold[plot]
];


"### Import Notebook";
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

"### Generate GIF From Manipulate[]";
Utils`manipulateGif[manipulate_, name_String, step_Integer] := Export[
    name <> ".gif",
    Import[
        Export[name <> ".avi", manipulate]
        , "ImageList"
    ][[1 ;; -1 ;; step]]
];

"### Show Exec Time & Pass Output";
SetAttributes[Utils`timeExec, {HoldAll, SequenceHold}];
Utils`timeExec[operations__] := (
   Print[Now];
   (Print[#1]; #2) & @@ Timing[operations]
);

"### Print Previous Result";
Utils`printPrevious := Print[%];

"### Inspect Variable";
SetAttributes[Utils`printName, HoldAll];
Utils`printName[var_] := Print[{
    SymbolName[Unevaluated[var]], var
}];


End[] (* End `Private` *)
EndPackage[]


"### Show public Information";
Column[{
        Style[NotebookFileName[], Bold, Larger]
        , Information["Utils`*"]
    }
    , Frame -> True, FrameStyle -> Transparent
    (* , Spacings -> {Automatic, {2, 1}} *)
] // hideShow
