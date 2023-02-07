(* ::Package:: *)

(* ::Title:: *)
(*GRUtils*)


(* ::Text:: *)
(*For an up-to-date version, go to https://github.com/bryango/Physica*)


(* ::Section:: *)
(*Basics*)


lengthToMetric = Function[ {quadraticForm, coord},
    Table[
        (1/2) D[quadraticForm, Dt[i], Dt[j]],
        {i, coord}, {j, coord}
    ]
];



(* ::Section:: *)
(* Coord transform *)


jacobianFromMap = Function[ {oldByNewMap, newCoord},
    Grad @@ (
        { oldByNewMap @@ #, # } & @ newCoord
    )
];

jacobian := jacobianFromMap;

newMetric[
    oldMetric_List, oldCoord_List,
    oldByNewMap_, newCoord_List
] := # . (
    oldMetric /. (
        #1 -> #2 & @@@
        Transpose[{oldCoord, oldByNewMap @@ newCoord}]
    )
) . # & @ (
      Dt[oldByNewMap @@ newCoord]
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





(* vim: set ts=4 sw=4: *)
