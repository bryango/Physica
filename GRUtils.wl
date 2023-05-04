(* ::Package:: *)

(* ::Title:: *)
(*GRUtils*)


(* ::Subtitle:: *)
(*For an up-to-date version, go to https://github.com/bryango/Physica*)


(* ::Item:: *)
(*Pretty-differentials:*)


(* ::Subitem:: *)
(*The pretty-printing is tagged to `MakeBoxes`, not `Dt`*)


(* ::Subitem:: *)
(*... because `Dt` is a Protected Symbol*)


ClearAll[DifferentialD]
MakeBoxes[Dt[x_], StandardForm] := RowBox @ {"\[DifferentialD]", ToBoxes[x]}
DifferentialD := Dt

(*?? MakeBoxes*)


(* ::Item:: *)
(*Experimental: Make \[TensorWedge] work for abstract tensors:*)


(*
TensorWedge;  (*(*(* trigger system default autoload *)*)*)
Unprotect[TensorWedge];
    TensorWedge[x_ + y_, z_] := TensorWedge[x, z] + TensorWedge[y, z]
    TensorWedge /: TensorWedge[x_, y_] + TensorWedge[y_, x_] := 0
    (* SetAttributes[TensorWedge, ReadProtected]; *)
Protect[TensorWedge];
*)


lengthToMetric = Function[ {quadraticForm, coord},
    Table[
        (1/2) D[quadraticForm, Dt[i], Dt[j]],
        {i, coord}, {j, coord}
    ]
];

metricToLength = Function[ {metric, coord},
    Dt[coord] . metric . Dt[coord]
];


jacobianFromMap = Function[ {upByDownMap, downCoord},
    Grad @@ (
        { upByDownMap @@ #, # } & @ downCoord
    )
];

jacobian := jacobianFromMap;


ClearAll @ labelContract
labelContract[indices__] := Function[tensor,
    Fold[
        Sum[#1, {#2, 1, dim}] &
        , tensor
        , {indices}
    ]
];


(* vim: set ts=4 sw=4: *)
