#!/usr/bin/env wolframscript
(* ::Package:: *)
(* For an up-to-date version, go to:
    https://github.com/bryango/Templates > Mathematica
*)

BeginPackage["GRUtils`"]
`Private`packageName = Context[];

Begin["`Private`"] (* Un-scoped variables defaults to `Private` *)


(* ::Section:: *)
(* Coord transform *)

GRUtils`jacobianFromLists[
    up_List, down_List
] := Outer[
    D[ up[[#1]], down[[#2]] ] &,
    Range[Length[up]], Range[Length[down]]
];

GRUtils`jacobianFromFunc[oldByNewFunc_, newCoord_List] :=
jacobianFromLists @@ (
    { oldByNewFunc @@ #, # } & @ newCoord
);

GRUtils`jacobian := GRUtils`jacobianFromFunc;

GRUtils`newMetric[
    oldMetric_List, oldCoord_List,
    oldByNewFunc_, newCoord_List
] := # . (
    oldMetric /. (
        #1 -> #2 & @@@
        Transpose[{oldCoord, oldByNewFunc @@ newCoord}]
    )
) . # & @ (
      Dt[oldByNewFunc @@ newCoord]
) // Simplify // quadToMatrix[#, newCoord] &;


(* ::Section:: *)
(* Other utils *)

"## sum over component labels;"
GRUtils`labelContract[indices__] := Function[tensor,
    Fold[
        Sum[#1, {#2, 1, dim}] &
        , tensor
        , {indices}
    ]
];


"## finds the metric tensor for a quadratic form";
"## ... reference: ccgrg";
GRUtils`quadToMatrix[form_, variables_] := Module[{
        Dx = Dt /@ variables,
        dimension = Length[variables],
        g, gArray, entries
    },
	gArray = Array[g, {dimension, dimension}];
    g[i_,k_] := g[k,i] /; k<i;
	entries = DeleteDuplicates[Flatten[gArray]];

    linearSystem = ( # == 0 ) &@ DeleteCases[
        Flatten[CoefficientList[
            Simplify[form] - Simplify[Dx.gArray.Dx],
            Dx
        ]],
        0 (* remove trivial equations *)
    ];
	gArray = gArray /. Flatten[
        Solve[linearSystem, entries]
    ];
	Simplify[gArray]
];


(* ::Section:: *)
(* Ending *)

End[] (* End `Private` *)
EndPackage[]

"### show public information";
If[ ! inspectUtils,
    "DoNothing";
    , ReleaseHold[#], ReleaseHold[#]
] & @ Hold[
    Information["GRUtils`*"];
]


(* vim: set ts=4 sw=4: *)
