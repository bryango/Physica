(* ::Package:: *)

(* ::Title:: *)
(*GRUtils*)


(* ::Subtitle:: *)
(*For an up-to-date version, go to https://github.com/bryango/Physica*)


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
