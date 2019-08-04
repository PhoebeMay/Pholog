
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VAR of (
# 8 "parser.mly"
       (string)
# 11 "parser.ml"
  )
    | RBRAC
    | QUES
    | NAME of (
# 8 "parser.mly"
       (string)
# 18 "parser.ml"
  )
    | LBRAC
    | FSTOP
    | END
    | COMMA
    | ARR
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState29
  | MenhirState25
  | MenhirState20
  | MenhirState17
  | MenhirState10
  | MenhirState6
  | MenhirState3
  | MenhirState1
  | MenhirState0

# 4 "parser.mly"
  
  open ParseTree

# 55 "parser.ml"

let rec _menhir_goto_sentence : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sentence -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_sentence) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | QUES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_sentence) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | NAME _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv128)
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_sentence) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_sentence)) = _menhir_stack in
            let _v : 'tv_program = 
# 20 "parser.mly"
             (Program(_1, Resolvant([])))
# 86 "parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_sentence) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)) : 'freshtv134)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv137 * _menhir_state * 'tv_clause) * _menhir_state * 'tv_sentence) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state * 'tv_clause) * _menhir_state * 'tv_sentence) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_clause)), _, (_2 : 'tv_sentence)) = _menhir_stack in
        let _v : 'tv_sentence = 
# 27 "parser.mly"
                    (addClause _2 _1)
# 105 "parser.ml"
         in
        _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv136)) : 'freshtv138)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_program -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_program) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_program) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_program) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_program)) = _menhir_stack in
        let _2 = () in
        let _v : (
# 11 "parser.mly"
      (string ParseTree.program)
# 129 "parser.ml"
        ) = 
# 16 "parser.mly"
                (_1)
# 133 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 11 "parser.mly"
      (string ParseTree.program)
# 141 "parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 11 "parser.mly"
      (string ParseTree.program)
# 149 "parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 11 "parser.mly"
      (string ParseTree.program)
# 157 "parser.ml"
        )) : (
# 11 "parser.mly"
      (string ParseTree.program)
# 161 "parser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv114)) : 'freshtv116)) : 'freshtv118)) : 'freshtv120)) : 'freshtv122)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_program) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)) : 'freshtv126)

and _menhir_goto_term_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv99 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 181 "parser.ml"
        ))) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRAC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv95 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 191 "parser.ml"
            ))) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv93 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 198 "parser.ml"
            ))) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "parser.mly"
       (string)
# 203 "parser.ml"
            ))), _, (_3 : 'tv_term_list)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_term = 
# 44 "parser.mly"
                               (TFun(_1, _3))
# 210 "parser.ml"
             in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)) : 'freshtv96)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv97 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 220 "parser.ml"
            ))) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv103 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv101 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_term)), _, (_3 : 'tv_term_list)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_term_list = 
# 48 "parser.mly"
                         (_1::_3)
# 234 "parser.ml"
         in
        _menhir_goto_term_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv102)) : 'freshtv104)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv111 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 242 "parser.ml"
        ))) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRAC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv107 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 252 "parser.ml"
            ))) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv105 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 259 "parser.ml"
            ))) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "parser.mly"
       (string)
# 264 "parser.ml"
            ))), _, (_3 : 'tv_term_list)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_atom = 
# 38 "parser.mly"
                               (Atom(_1, _3))
# 271 "parser.ml"
             in
            _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv106)) : 'freshtv108)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv109 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 281 "parser.ml"
            ))) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)) : 'freshtv112)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_clause : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_clause -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv91 * _menhir_state * 'tv_clause) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NAME _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | END | QUES ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * 'tv_clause) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_clause)) = _menhir_stack in
        let _v : 'tv_sentence = 
# 28 "parser.mly"
                    (Sentence([_1]))
# 310 "parser.ml"
         in
        _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv92)

and _menhir_goto_atom_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atom_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState20 | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_atom_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_atom_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_atom_list)) = _menhir_stack in
        let _v : 'tv_resolvant = 
# 24 "parser.mly"
                  (Resolvant(_1))
# 331 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_resolvant) = _v in
        ((match _menhir_s with
        | MenhirState1 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_resolvant) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_2 : 'tv_resolvant) : 'tv_resolvant) = _v in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_program = 
# 21 "parser.mly"
                   (Program(Sentence([]),_2))
# 352 "parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv64)) : 'freshtv66)
        | MenhirState20 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_sentence)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_resolvant) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_sentence)) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_3 : 'tv_resolvant) : 'tv_resolvant) = _v in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_sentence)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_program = 
# 19 "parser.mly"
                          (Program(_1, _3))
# 369 "parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv68)) : 'freshtv70)
        | _ ->
            _menhir_fail ()) : 'freshtv72)) : 'freshtv74)) : 'freshtv76)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv79 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv77 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_atom)), _, (_3 : 'tv_atom_list)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_atom_list = 
# 36 "parser.mly"
                         (_1::_3)
# 384 "parser.ml"
         in
        _menhir_goto_atom_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv78)) : 'freshtv80)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv83 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv81 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom_list) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_atom)), _, (_3 : 'tv_atom_list)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_clause = 
# 31 "parser.mly"
                             (Clause(_1,_3))
# 405 "parser.ml"
             in
            _menhir_goto_clause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)) : 'freshtv84)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv85 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)
    | _ ->
        _menhir_fail ()

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | NAME _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv56)
    | RBRAC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_term)) = _menhir_stack in
        let _v : 'tv_term_list = 
# 47 "parser.mly"
         ([_1])
# 447 "parser.ml"
         in
        _menhir_goto_term_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv58)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)) : 'freshtv62)

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 | MenhirState20 | MenhirState17 | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | NAME _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv38)
        | END | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_atom_list = 
# 35 "parser.mly"
         ([_1])
# 487 "parser.ml"
             in
            _menhir_goto_atom_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv41 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)) : 'freshtv44)
    | MenhirState0 | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv45 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | NAME _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv46)
        | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv47 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_atom)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_clause = 
# 32 "parser.mly"
               (Clause(_1,[]))
# 526 "parser.ml"
             in
            _menhir_goto_clause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv48)) : 'freshtv50)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)) : 'freshtv54)
    | _ ->
        _menhir_fail ()

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 542 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 8 "parser.mly"
       (string)
# 552 "parser.ml"
    )) : (
# 8 "parser.mly"
       (string)
# 556 "parser.ml"
    )) = _v in
    ((let _v : 'tv_term = 
# 42 "parser.mly"
        (TVar(_1))
# 561 "parser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 568 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRAC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 580 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | NAME _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv30)
    | COMMA | RBRAC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 598 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 8 "parser.mly"
       (string)
# 603 "parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_term = 
# 43 "parser.mly"
         (TFun(_1,[]))
# 608 "parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 618 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv11 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * 'tv_clause) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv15 * _menhir_state * 'tv_sentence)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 656 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 665 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv28)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 682 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRAC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 694 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | NAME _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv6)
    | ARR | COMMA | END | FSTOP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 712 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 8 "parser.mly"
       (string)
# 717 "parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_atom = 
# 39 "parser.mly"
           (Atom(_1, []))
# 722 "parser.ml"
         in
        _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 732 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 11 "parser.mly"
      (string ParseTree.program)
# 752 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NAME _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | QUES ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | NAME _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv4))

# 219 "/usr/share/menhir/standard.mly"
  


# 796 "parser.ml"
