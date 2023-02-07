#!/usr/bin/env wolframscript
(* ::Package:: *)
(* For an up-to-date version, go to:
    https://github.com/bryango/Physica
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
) // Simplify // lengthToMetric[#, newCoord] &;


(* ::Section:: *)
(* Other utils *)

"## sum over component labels";
GRUtils`labelContract[indices__] := Function[tensor,
    Fold[
        Sum[#1, {#2, 1, dim}] &
        , tensor
        , {indices}
    ]
];


lengthToMetric = Function[ {quadraticForm, coord},
    Table[
        (1/2) D[quadraticForm, Dt[i], Dt[j]],
        {i, coord}, {j, coord}
    ]
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
    Information["GRUtils`*"]
]


(* vim: set ts=4 sw=4: *)
