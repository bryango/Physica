#!/usr/bin/env wolframscript
(* ::Package:: *)
(* For an up-to-date version, go to:
    https://github.com/bryango/Physica
*)

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
            ReleaseHold@MakeExpression@boxedExpr
        ]
    ],
    {General::shdw}
]
