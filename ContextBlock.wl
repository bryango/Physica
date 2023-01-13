(* ::Package:: *)

(* ::Title:: *)
(*Context Block*)


(* ::Item:: *)
(*For an up-to-date version, go to: https://github.com/bryango/Physica. *)


(* ::Section:: *)
(*Notes*)


(* ::Item:: *)
(*Fact: context resolutions are performed immediately at the parsing stage,*)
(*before actual evaluations.*)


(* ::Subitem:: *)
(*See: https://stackoverflow.com/questions/7917550 *)


(* ::Subitem:: *)
(*Therefore, it is impossible to use relative contexts (`...),*)
(*unless at top level. So relative contexts do not work in Blocks.*)
(*This is consistent with the syntax highlighting!*)


(* ::Subitem:: *)
(*However, it is possible to impose the expected contexts,*)
(*by delaying the parsing stage. This is implemented here.*)


(* ::Subitem:: *)
(*Caveat: Global` is then polluted with the added symbols,*)
(*but fortunately without overwriting Global` definitions.*)


(* ::Section:: *)
(*Usage*)


(* ::Item:: *)
(*To use the following context blocks,*)
(*we need to first populate the context` with symbol definitions.*)
(*Here an explicit context` is required,*)
(*otherwise the context`of a symbol will be parsed to Global`.*)


(* ::Item:: *)
(*We then use those symbols to compute quantities.*)
(*Now the implicit context` is in effect,*)
(*so one does not need to write the full symbol name.*)


(* ::Section:: *)
(* Context Management *)


SetAttributes[noGlobalBlock, HoldAll];
noGlobalBlock[expr_] := Block[
    {
        (*(*(* avoid polluting the Global` context *)*)*)
        $ContextPath = DeleteCases["Global`"]@$ContextPath
    },
    expr
];


SetAttributes[contextBlock, HoldRest];
contextBlock[context_, expr_] := Quiet[
    With[{ boxedExpr = MakeBoxes[expr] },
        Block[
            {
                (*(*(* context` symbols take precedence *)*)*)
                $ContextPath = Prepend[context]@$ContextPath,
                
                (*(*(* new symbols defined in context` *)*)*)
                $Context = context,
                
                (*(*(* tmp for convenience *)*)*)
                tmp
            },

            (*(*(* semi-implicit contexting *)*)*)
            ReleaseHold@MakeExpression@boxedExpr
        ]
    ],
    {General::shdw}
]
