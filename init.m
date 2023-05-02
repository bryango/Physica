#!/usr/bin/env wolframscript
(* ::Package:: *)
(* Linked to `~/.Mathematica/Autoload/FrontEnd/init.m` *)

"### Spelunking";
Quiet[Get["Spelunking`"];];

(*
Quiet[Get["GeneralUtilities`"];];
Unprotect[GeneralUtilities`Definitions];
GeneralUtilities`Definitions[
    GeneralUtilities`Code`PackagePrivate`sym_Symbol
] := Internal`UnsafeQuietCheck[
    Join @@ DeleteCases[{}][
        (#1[GeneralUtilities`Code`PackagePrivate`sym] &) /@ {
            DownValues,
            DefaultValues,
            GeneralUtilities`PackageScope`OwnDefs,
            SubValues,
            GeneralUtilities`Code`PackagePrivate`UpDefs,
            FormatValues,
            GeneralUtilities`Code`PackagePrivate`AttributeDefs,
            GeneralUtilities`Code`PackagePrivate`OptionDefs,
            GeneralUtilities`Code`PackagePrivate`KernelPlaceholderDefs
        }
    ],
    {}
];
Protect[GeneralUtilities`Definitions];
*)

SetOptions[Language`ExtendedDefinition, ExcludedContexts -> {}];
SetOptions[Language`ExtendedFullDefinition, ExcludedContexts -> {}];

"### Beep";
$Post = (Beep[];(*Speak["Wow"];*)#) &;
(* $Post =.; *)

"### Autosave";
SetOptions[$FrontEndSession, NotebookAutoSave -> True]

(*
"### Add quit kernel shortcut";
FrontEndExecute[
    FrontEnd`AddMenuCommands["MenuListQuitEvaluators", {MenuItem[
        "Quit Default"
        , FrontEnd`KernelExecute[ToExpression["Quit[]"]]
        , MenuKey["q", Modifiers -> {"Control", "Shift"}]
        , System`MenuEvaluator -> Automatic
]}]]
*)
