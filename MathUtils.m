BeginPackage["Utils`"]
packageName = Context[];

Utils::usage =
    "Useful utilities for generic notebooks."


"### Context Management";
Funcs`prependContext[context_] := If[ ! MemberQ[$ContextPath, context],
    PrependTo[$ContextPath, context];
];
"# Access Funcs directly, within package";
Funcs`prependContext["Funcs`"];


"### Auto Collapse Cell";
Funcs`autoCollapse[] :=(
    If[ $FrontEnd =!= $Failed,
        SelectionMove[EvaluationNotebook[], All, GeneratedCell];
        FrontEndTokenExecute["SelectionCloseUnselectedCells"]
    ]
);

SetAttributes[Funcs`hideShow, HoldFirst];
Funcs`hideShow[x_, styles_: {}] := (
    Print[
        Style[x, Sequence @@ styles]
    ];
    autoCollapse[];
);

SetAttributes[Funcs`hideInfo, HoldFirst];
Funcs`hideInfo[info_, styles_: {}] := hideShow[
    "### " <> ToString[info],
    {Bold}~Join~styles
];


"### Hold Items for Further processing";
SetAttributes[Funcs`holdItems, HoldAll]
Funcs`holdItems[list_] := ReleaseHold[
    MapAt[HoldForm, Hold[list], {All, All}]
];


"### Plot Utils";
Funcs`colorPalette[theme_] := (
    ("DefaultPlotStyle" /. (
        Method /. Charting`ResolvePlotTheme[theme, ListPlot]
    )) /. Directive[x_, __] :> x
);

Funcs`getPlotPoints[plot_] := Cases[
    plot, Line[pts_] :> pts, Infinity
];


EndPackage[]


"### Access Funcs directly, in parent namespace";
Funcs`prependContext["Funcs`"];
