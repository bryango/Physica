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

If[$Notebooks, SetDirectory@NotebookDirectory[]]
(* SetOptions[EvaluationNotebook[], Magnification -> .9] *)
(* Options[EvaluationNotebook[], CellContext] *)

(* Off[Limit::alimv] *)


(* ::Item:: *)
(*Piecewise defaults to Indeterminate:*)


Unprotect[Piecewise];
    Piecewise[a_List] := Piecewise[a, Indeterminate];
    Piecewise /: Default[Piecewise, 2] := Indeterminate;
Protect[Piecewise];


(* ::Subitem:: *)
(*Note that Default[Piecewise, 2] := Indeterminate doesn't actually work,*)
(*... as Piecewise[___] is a kernel function.*)
(*But we set it anyways for consistency.*)


(* ::Item:: *)
(*Allows Subscript-ing defined symbols:*)


ClearAll[Subscript]
SetAttributes[Subscript, HoldFirst]


(* ::Section:: *)
(* Mathematics *)


addAssumptions = Catch[
    If[Simplify[And @@ {##}], Throw[$Assumptions]];
    $Assumptions = $Assumptions && Simplify[And @@ {##}]
] &;


ClearAll[redify]
MakeBoxes[redify[x_], StandardForm] ^:= ToBoxes[Style[x, Red]]
unred = ReplaceAll[redify -> (# &)];


ClearAll[unNest]
unNest[f_ : List] := ReplaceRepeated[f@f@x___ :> f@x]
unpack := ReplaceAll[ {x_} :> x ] @* unNest[List]


ClearAll[actOnNumeratorDenominator, depressMinus]
actOnNumeratorDenominator[func_] :=
    Apply[#1/#2 &] @* func @* NumeratorDenominator
depressMinus := (-# &) @* actOnNumeratorDenominator[Apply[{-#1, #2} &]]


SetAttributes[holdItems, HoldAll]
holdItems[list_] := ReleaseHold[
    MapAt[Hold, Hold[list], {All, All}]
];


collectCoefficient[expr_, coefficient_, showForm_: Null] :=
Module[{form = showForm, remaining},
    If[MatrixQ[expr] && form == Null, form = MatrixForm];
    If[form == Null, form = HoldForm @* Evaluate @* Simplify];
    HoldForm[ HoldForm[coefficient] remaining ] /. {
        remaining -> form[expr/coefficient]
    }
];


ClearAll[timedSimplify];
Options[timedSimplify] = { TimeConstraint -> {1, 30} };

timedSimplify[
    expr: _,
    addAssumpt: Except[OptionsPattern[]] | _List?(Flatten[#] == {} &) : {},
    timeout: Except[OptionsPattern[]] : OptionValue[timedSimplify, TimeConstraint],
    opts: OptionsPattern @ {timedSimplify, FullSimplify}
] := Quiet[
    FullSimplify[
        expr, addAssumpt, opts,
        TimeConstraint -> timeout,
        Sequence @@ Options[timedSimplify]
    ],
    {FullSimplify::time, FullSimplify::gtime}
];

timedSimplify::usage="timedSimplify[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"addAssumpt\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"timeout\",\nFontSlant->\"Italic\"]\), ...] calls FullSimplify[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"addAssumpt\",\nFontSlant->\"Italic\"]\), ..., TimeConstraint \[Rule] \!\(\*
StyleBox[\"timeout\",\nFontSlant->\"Italic\"]\)], where \!\(\*
StyleBox[\"timeout\",\nFontSlant->\"Italic\"]\) is optional and defaults to OptionValue[timedSimplify, TimeConstraint], while \!\(\*
StyleBox[\"addAssumpt\",\nFontSlant->\"Italic\"]\) specifies the additional assumptions on top of $Assumptions.";


(* ::Item:: *)
(*Currying, i.e. constructing the operator form of a function:*)


(* ::Subitem:: *)
(*https://stackoverflow.com/a/11561797*)


(* ::Subitem:: *)
(*https://mathematica.stackexchange.com/a/23831*)


(* ::Subitem:: *)
(*Documentation: Function > Details*)


ClearAll[act];
SetAttributes[act, HoldAll];

act[f_[opts___]] := Function[
    Null,                     (*(*(* accepting # slot *)*)*)
    f[SlotSequence[], opts],  (*(*(* ... of some ## sequence *)*)*)
    HoldAll                   (*(*(* ... with the attribute *)*)*)
];

act::usage="act@\!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\)[\!\(\*
StyleBox[\"opts\",\nFontSlant->\"Italic\"]\)] generates the operator form: \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\)[##, \!\(\*
StyleBox[\"opts\",\nFontSlant->\"Italic\"]\)]&, where the first few arguments of \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\) are \[OpenCurlyDoubleQuote]curried\[CloseCurlyDoubleQuote], while the remaining arguments are filled in with \!\(\*
StyleBox[\"opts\",\nFontSlant->\"Italic\"]\). The resulting operator \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\)[##, \!\(\*
StyleBox[\"opts\",\nFontSlant->\"Italic\"]\)]& has attribute HoldAll, namely the argument sequence ## is Unevaluated when passed into the operator.";


ClearAll["then"];
SetAttributes[then, HoldAll];

f_[then,opts__] ^:= then[
    Function[Null, f[SlotSequence[], opts], HoldAll]
] /; Not @ MatchQ[
    (*(*(* avoid key internal functions *)*)*)
    f, Clear|ClearAll|MessageName
];

f_[then[g_], opts__] ^:= then[
    g /* Function[Null, f[SlotSequence[], opts], HoldAll]
];

then[f_][x___] := f[x];  (*(*(* x is NOT held *)*)*)

then::usage="then is a placeholder similar to the SlotSequence ##. \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\)[then, \!\(\*
StyleBox[\"opts\",\nFontSlant->\"Italic\"]\)] is equivalent to the operator \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\)[##, \!\(\*
StyleBox[\"opts\",\nFontSlant->\"Italic\"]\)] &. When acted on the argument sequence, the operator awaits its evaluation, and then performs the operations afterwards. Some key internal functions like ClearAll are blacklisted, as such \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\) may lead to unexpected results. For more complicated effects, use slots (# and ##) instead. ";


ClearAll[fixed];
fixed[f_] := FixedPoint[f, ##] &


(* ::Section:: *)
(* Context Management *)


wipeAll[context_: "Global`"] := Quiet[
    # @@ Names[context ~~ "*" ~~ "`*" ...],
    {Remove::rmnsm, Remove::relex}
] & /@ { Unprotect, Remove }


prependContext[context_] := If[ ! First @ $ContextPath == context,
    PrependTo[$ContextPath, context];
];


(* ::Section:: *)
(* Inspections *)


echoWith = Function[echoFunction,
    Function[input,
        EchoFunction[echoFunction][Unevaluated@input],
        HoldAll
    ]
];
echoIn = echoWith[HoldForm];


(* ::Item:: *)
(*Post-processing / pretty-printing:*)


ClearAll[pp]
pp[ style_: Map @ MatrixForm, op_: FullSimplify] :=
    op /* EchoFunction[style];


SetAttributes[printName, HoldAll];
printName[var_] := Print[{
    SymbolName[Unevaluated[var]], var
}]


(*(*(* show exec time & pass output *)*)*)
SetAttributes[timeExec, {HoldAll, SequenceHold}];
timeExec[operations__] := (
   Print[Now];
   (Print[#1]; #2) & @@ Timing[operations]
);


(* ::Section:: *)
(* Import & Export *)


ClearAll[getNotebook]
getNotebook[nbFile__] := NotebookEvaluate[
    NotebookOpen[
        FileNames[nbFile] // Sort // Last // FindFile,
        CellContext -> "Global`"
    ],
    InsertResults -> True,
    EvaluationElements -> "InitializationCell"
]
(*(*(* https://mathematica.stackexchange.com/a/111850 *)*)*)


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
