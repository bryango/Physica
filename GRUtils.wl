#!/usr/bin/env wolframscript
(* ::Package:: *)
(* For an up-to-date version, go to:
    https://github.com/bryango/Physica
*)

(* ::Section:: *)
(* Coord transform *)

jacobianFromLists[
    up_List, down_List
] := Outer[
    D[ up[[#1]], down[[#2]] ] &,
    Range[Length[up]], Range[Length[down]]
];

jacobianFromFunc[oldByNewFunc_, newCoord_List] :=
jacobianFromLists @@ (
    { oldByNewFunc @@ #, # } & @ newCoord
);

jacobian := jacobianFromFunc;

newMetric[
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
labelContract[indices__] := Function[tensor,
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


(* vim: set ts=4 sw=4: *)
