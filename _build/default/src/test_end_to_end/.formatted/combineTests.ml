open OUnit
open Dt
open Runall
open NoTypeCheck
open TypeCheckTests

exception UnifiyFail

exception False

let suite = "OUnit Lexer + Parser Tests" >::: tests_notc @ tests_tc
