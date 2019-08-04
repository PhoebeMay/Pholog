module MenhirBasics = struct
  exception Error

  type token =
    | VAR of string
    | TYPEDEF
    | RBRAC
    | QUES
    | PRED
    | PLUS
    | NAME of string
    | MINUS
    | LBRAC
    | IS
    | INTTYP
    | INT of int
    | FSTOP
    | FAIL
    | EQUALS
    | END
    | CUT
    | COMMA
    | ARR
end

include MenhirBasics

let _eRR = MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer : Lexing.lexbuf -> token;
  _menhir_lexbuf : Lexing.lexbuf;
  _menhir_token : token;
  mutable _menhir_error : bool;
}

and _menhir_state =
  | MenhirState82
  | MenhirState78
  | MenhirState71
  | MenhirState66
  | MenhirState60
  | MenhirState51
  | MenhirState46
  | MenhirState43
  | MenhirState40
  | MenhirState38
  | MenhirState31
  | MenhirState29
  | MenhirState26
  | MenhirState21
  | MenhirState16
  | MenhirState11
  | MenhirState8
  | MenhirState6
  | MenhirState4
  | MenhirState2
  | MenhirState0

open ParseTree

let rec _menhir_goto_new_type :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_new_type -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let (_v : 'tv_new_type) = _v in
  ( let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_new_type) : 'tv_new_type) = _v in
    ( let _v : 'tv_sentence_item = D _1 in
      _menhir_goto_sentence_item _menhir_env _menhir_stack _menhir_s _v
      : 'freshtv342 )
    : 'freshtv344 )

and _menhir_goto_typedef_list :
    _menhir_env ->
    'ttv_tail ->
    _menhir_state ->
    'tv_typedef_list ->
    'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState21 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv323 * _menhir_state * 'tv_typedef_list_elem)
              * _menhir_state
              * 'tv_typedef_list) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : ('freshtv321 * _menhir_state * 'tv_typedef_list_elem)
                * _menhir_state
                * 'tv_typedef_list) =
          Obj.magic _menhir_stack
        in
        ( let ( (_menhir_stack, _menhir_s, (_1 : 'tv_typedef_list_elem)),
                _,
                (_3 : 'tv_typedef_list) ) =
            _menhir_stack
          in
          let _2 = () in
          let _v : 'tv_typedef_list = _1 :: _3 in
          _menhir_goto_typedef_list _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv322 )
        : 'freshtv324 )
  | MenhirState6 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ((('freshtv331 * _menhir_state) * string) * _menhir_state)
              * _menhir_state
              * 'tv_typedef_list) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ((('freshtv327 * _menhir_state) * string) * _menhir_state)
                    * _menhir_state
                    * 'tv_typedef_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack
                    : ((('freshtv325 * _menhir_state) * string) * _menhir_state)
                      * _menhir_state
                      * 'tv_typedef_list) =
                Obj.magic _menhir_stack
              in
              ( let ( (((_menhir_stack, _menhir_s), (_2 : string)), _),
                      _,
                      (_4 : 'tv_typedef_list) ) =
                  _menhir_stack
                in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_new_type = TypeDef (_2, [], _4) in
                _menhir_goto_new_type _menhir_env _menhir_stack _menhir_s _v
                : 'freshtv326 )
              : 'freshtv328 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ((('freshtv329 * _menhir_state) * string) * _menhir_state)
                    * _menhir_state
                    * 'tv_typedef_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv330 )
        : 'freshtv332 )
  | MenhirState26 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ( (('freshtv339 * _menhir_state) * string)
              * _menhir_state
              * 'tv_var_list )
              * _menhir_state
              * 'tv_typedef_list) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ( (('freshtv335 * _menhir_state) * string)
                    * _menhir_state
                    * 'tv_var_list )
                    * _menhir_state
                    * 'tv_typedef_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack
                    : ( (('freshtv333 * _menhir_state) * string)
                      * _menhir_state
                      * 'tv_var_list )
                      * _menhir_state
                      * 'tv_typedef_list) =
                Obj.magic _menhir_stack
              in
              ( let ( ( ((_menhir_stack, _menhir_s), (_2 : string)),
                        _,
                        (_3 : 'tv_var_list) ),
                      _,
                      (_5 : 'tv_typedef_list) ) =
                  _menhir_stack
                in
                let _6 = () in
                let _4 = () in
                let _1 = () in
                let _v : 'tv_new_type = TypeDef (_2, _3, _5) in
                _menhir_goto_new_type _menhir_env _menhir_stack _menhir_s _v
                : 'freshtv334 )
              : 'freshtv336 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ( (('freshtv337 * _menhir_state) * string)
                    * _menhir_state
                    * 'tv_var_list )
                    * _menhir_state
                    * 'tv_typedef_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv338 )
        : 'freshtv340 )
  | _ -> _menhir_fail ()

and _menhir_goto_arith :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arith -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState31 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv311 * _menhir_state * string)
              * _menhir_state
              * 'tv_arith) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_arith) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              match _tok with
              | INT _v ->
                  _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState40 _v
              | MINUS ->
                  _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState40
              | VAR _v ->
                  _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState40 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                    MenhirState40
              : 'freshtv298 )
        | PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_arith) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              match _tok with
              | INT _v ->
                  _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState38 _v
              | MINUS ->
                  _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState38
              | VAR _v ->
                  _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState38 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                    MenhirState38
              : 'freshtv300 )
        | COMMA | END | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv307 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_arith) =
              Obj.magic _menhir_stack
            in
            ( let (_menhir_stack, _menhir_s, (_1 : string)), _, (_3 : 'tv_arith)
                  =
                _menhir_stack
              in
              let _2 = () in
              let _v : 'tv_is_expr = IsExpr (_1, _3) in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv305) = _menhir_stack in
              let (_menhir_s : _menhir_state) = _menhir_s in
              let (_v : 'tv_is_expr) = _v in
              ( let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv303) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_is_expr) = _v in
                ( let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : 'freshtv301) =
                    Obj.magic _menhir_stack
                  in
                  let (_menhir_s : _menhir_state) = _menhir_s in
                  let ((_1 : 'tv_is_expr) : 'tv_is_expr) = _v in
                  ( let _v : 'tv_clause_body_one = CAR _1 in
                    _menhir_goto_clause_body_one _menhir_env _menhir_stack
                      _menhir_s _v
                    : 'freshtv302 )
                  : 'freshtv304 )
                : 'freshtv306 )
              : 'freshtv308 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv309 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_arith) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv310 )
        : 'freshtv312 )
  | MenhirState38 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv315 * _menhir_state * 'tv_arith)
              * _menhir_state
              * 'tv_arith) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : ('freshtv313 * _menhir_state * 'tv_arith)
                * _menhir_state
                * 'tv_arith) =
          Obj.magic _menhir_stack
        in
        ( let (_menhir_stack, _menhir_s, (_1 : 'tv_arith)), _, (_3 : 'tv_arith)
              =
            _menhir_stack
          in
          let _2 = () in
          let _v : 'tv_arith = Plus (_1, _3) in
          _menhir_goto_arith _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv314 )
        : 'freshtv316 )
  | MenhirState40 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv319 * _menhir_state * 'tv_arith)
              * _menhir_state
              * 'tv_arith) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : ('freshtv317 * _menhir_state * 'tv_arith)
                * _menhir_state
                * 'tv_arith) =
          Obj.magic _menhir_stack
        in
        ( let (_menhir_stack, _menhir_s, (_1 : 'tv_arith)), _, (_3 : 'tv_arith)
              =
            _menhir_stack
          in
          let _2 = () in
          let _v : 'tv_arith = Subtract (_1, _3) in
          _menhir_goto_arith _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv318 )
        : 'freshtv320 )
  | _ -> _menhir_fail ()

and _menhir_goto_program :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_program -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv295 * _menhir_state * 'tv_program) =
    Obj.magic _menhir_stack
  in
  ( assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * 'tv_program) =
          Obj.magic _menhir_stack
        in
        ( let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv289 * _menhir_state * 'tv_program) =
            Obj.magic _menhir_stack
          in
          ( let _menhir_stack, _menhir_s, (_1 : 'tv_program) = _menhir_stack in
            let _2 = () in
            let _v : string ParseTree.program = _1 in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv287) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : string ParseTree.program) = _v in
            ( let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv285) = Obj.magic _menhir_stack in
              let (_menhir_s : _menhir_state) = _menhir_s in
              let (_v : string ParseTree.program) = _v in
              ( let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : string ParseTree.program)
                      : string ParseTree.program) =
                  _v
                in
                (Obj.magic _1 : 'freshtv284)
                : 'freshtv286 )
              : 'freshtv288 )
            : 'freshtv290 )
          : 'freshtv292 )
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293 * _menhir_state * 'tv_program) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
          : 'freshtv294 )
    : 'freshtv296 )

and _menhir_goto_typedef_body_list :
    _menhir_env ->
    'ttv_tail ->
    _menhir_state ->
    'tv_typedef_body_list ->
    'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState11 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv257 * _menhir_state * string)
              * _menhir_state
              * 'tv_typedef_body_list) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRAC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv253 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_typedef_body_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack
                    : ('freshtv251 * _menhir_state * string)
                      * _menhir_state
                      * 'tv_typedef_body_list) =
                Obj.magic _menhir_stack
              in
              ( let ( (_menhir_stack, _menhir_s, (_1 : string)),
                      _,
                      (_3 : 'tv_typedef_body_list) ) =
                  _menhir_stack
                in
                let _4 = () in
                let _2 = () in
                let _v : 'tv_typedef_body = TypeCons (_1, _3) in
                _menhir_goto_typedef_body _menhir_env _menhir_stack _menhir_s
                  _v
                : 'freshtv252 )
              : 'freshtv254 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv255 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_typedef_body_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv256 )
        : 'freshtv258 )
  | MenhirState16 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv261 * _menhir_state * 'tv_typedef_body)
              * _menhir_state
              * 'tv_typedef_body_list) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : ('freshtv259 * _menhir_state * 'tv_typedef_body)
                * _menhir_state
                * 'tv_typedef_body_list) =
          Obj.magic _menhir_stack
        in
        ( let ( (_menhir_stack, _menhir_s, (_1 : 'tv_typedef_body)),
                _,
                (_3 : 'tv_typedef_body_list) ) =
            _menhir_stack
          in
          let _2 = () in
          let _v : 'tv_typedef_body_list = _1 :: _3 in
          _menhir_goto_typedef_body_list _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv260 )
        : 'freshtv262 )
  | MenhirState8 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv269 * _menhir_state * string)
              * _menhir_state
              * 'tv_typedef_body_list) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRAC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv265 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_typedef_body_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack
                    : ('freshtv263 * _menhir_state * string)
                      * _menhir_state
                      * 'tv_typedef_body_list) =
                Obj.magic _menhir_stack
              in
              ( let ( (_menhir_stack, _menhir_s, (_1 : string)),
                      _,
                      (_3 : 'tv_typedef_body_list) ) =
                  _menhir_stack
                in
                let _4 = () in
                let _2 = () in
                let _v : 'tv_typedef_list_elem = TypeDefRight (_1, _3) in
                _menhir_goto_typedef_list_elem _menhir_env _menhir_stack
                  _menhir_s _v
                : 'freshtv264 )
              : 'freshtv266 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv267 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_typedef_body_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv268 )
        : 'freshtv270 )
  | MenhirState66 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : (('freshtv281 * _menhir_state) * string)
              * _menhir_state
              * 'tv_typedef_body_list) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRAC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : (('freshtv277 * _menhir_state) * string)
                    * _menhir_state
                    * 'tv_typedef_body_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              match _tok with
              | FSTOP ->
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack
                        : (('freshtv273 * _menhir_state) * string)
                          * _menhir_state
                          * 'tv_typedef_body_list) =
                    Obj.magic _menhir_stack
                  in
                  ( let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack
                          : (('freshtv271 * _menhir_state) * string)
                            * _menhir_state
                            * 'tv_typedef_body_list) =
                      Obj.magic _menhir_stack
                    in
                    ( let ( ((_menhir_stack, _menhir_s), (_2 : string)),
                            _,
                            (_4 : 'tv_typedef_body_list) ) =
                        _menhir_stack
                      in
                      let _6 = () in
                      let _5 = () in
                      let _3 = () in
                      let _1 = () in
                      let _v : 'tv_pred_def = PredDef (_2, _4) in
                      _menhir_goto_pred_def _menhir_env _menhir_stack _menhir_s
                        _v
                      : 'freshtv272 )
                    : 'freshtv274 )
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack
                        : (('freshtv275 * _menhir_state) * string)
                          * _menhir_state
                          * 'tv_typedef_body_list) =
                    Obj.magic _menhir_stack
                  in
                  ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                      _menhir_s
                    : 'freshtv276 )
              : 'freshtv278 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : (('freshtv279 * _menhir_state) * string)
                    * _menhir_state
                    * 'tv_typedef_body_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv280 )
        : 'freshtv282 )
  | _ -> _menhir_fail ()

and _menhir_goto_sentence :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sentence -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState71 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv241 * _menhir_state * 'tv_sentence_item)
              * _menhir_state
              * 'tv_sentence) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : ('freshtv239 * _menhir_state * 'tv_sentence_item)
                * _menhir_state
                * 'tv_sentence) =
          Obj.magic _menhir_stack
        in
        ( let ( (_menhir_stack, _menhir_s, (_1 : 'tv_sentence_item)),
                _,
                (_2 : 'tv_sentence) ) =
            _menhir_stack
          in
          let _v : 'tv_sentence = addClause _2 _1 in
          _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv240 )
        : 'freshtv242 )
  | MenhirState0 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_sentence) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | QUES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_sentence) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              match _tok with
              | CUT ->
                  _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState82
              | FAIL ->
                  _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState82
              | NAME _v ->
                  _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState82 _v
              | VAR _v ->
                  _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState82 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                    MenhirState82
              : 'freshtv244 )
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_sentence) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, (_1 : 'tv_sentence) =
                _menhir_stack
              in
              let _v : 'tv_program = Program (_1, Resolvant []) in
              _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
              : 'freshtv246 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_sentence) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv248 )
        : 'freshtv250 )
  | _ -> _menhir_fail ()

and _menhir_goto_term_list :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term_list -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState46 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv225 * _menhir_state * string)
              * _menhir_state
              * 'tv_term_list) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRAC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv221 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_term_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack
                    : ('freshtv219 * _menhir_state * string)
                      * _menhir_state
                      * 'tv_term_list) =
                Obj.magic _menhir_stack
              in
              ( let ( (_menhir_stack, _menhir_s, (_1 : string)),
                      _,
                      (_3 : 'tv_term_list) ) =
                  _menhir_stack
                in
                let _4 = () in
                let _2 = () in
                let _v : 'tv_term = TFun (_1, _3) in
                _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
                : 'freshtv220 )
              : 'freshtv222 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv223 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_term_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv224 )
        : 'freshtv226 )
  | MenhirState51 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv229 * _menhir_state * 'tv_term)
              * _menhir_state
              * 'tv_term_list) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : ('freshtv227 * _menhir_state * 'tv_term)
                * _menhir_state
                * 'tv_term_list) =
          Obj.magic _menhir_stack
        in
        ( let ( (_menhir_stack, _menhir_s, (_1 : 'tv_term)),
                _,
                (_3 : 'tv_term_list) ) =
            _menhir_stack
          in
          let _2 = () in
          let _v : 'tv_term_list = _1 :: _3 in
          _menhir_goto_term_list _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv228 )
        : 'freshtv230 )
  | MenhirState43 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv237 * _menhir_state * string)
              * _menhir_state
              * 'tv_term_list) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRAC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv233 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_term_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack
                    : ('freshtv231 * _menhir_state * string)
                      * _menhir_state
                      * 'tv_term_list) =
                Obj.magic _menhir_stack
              in
              ( let ( (_menhir_stack, _menhir_s, (_1 : string)),
                      _,
                      (_3 : 'tv_term_list) ) =
                  _menhir_stack
                in
                let _4 = () in
                let _2 = () in
                let _v : 'tv_atom = Atom (_1, _3) in
                _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v
                : 'freshtv232 )
              : 'freshtv234 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv235 * _menhir_state * string)
                    * _menhir_state
                    * 'tv_term_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv236 )
        : 'freshtv238 )
  | _ -> _menhir_fail ()

and _menhir_goto_var_list :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_var_list -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState4 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv211 * _menhir_state * string)
              * _menhir_state
              * 'tv_var_list) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : ('freshtv209 * _menhir_state * string)
                * _menhir_state
                * 'tv_var_list) =
          Obj.magic _menhir_stack
        in
        ( let (_menhir_stack, _menhir_s, (_1 : string)), _, (_3 : 'tv_var_list)
              =
            _menhir_stack
          in
          let _2 = () in
          let _v : 'tv_var_list = _1 :: _3 in
          _menhir_goto_var_list _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv210 )
        : 'freshtv212 )
  | MenhirState2 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : (('freshtv217 * _menhir_state) * string)
              * _menhir_state
              * 'tv_var_list) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : (('freshtv213 * _menhir_state) * string)
                    * _menhir_state
                    * 'tv_var_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              match _tok with
              | NAME _v ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState26 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                    MenhirState26
              : 'freshtv214 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : (('freshtv215 * _menhir_state) * string)
                    * _menhir_state
                    * 'tv_var_list) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv216 )
        : 'freshtv218 )
  | _ -> _menhir_fail ()

and _menhir_goto_typedef_list_elem :
    _menhir_env ->
    'ttv_tail ->
    _menhir_state ->
    'tv_typedef_list_elem ->
    'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_typedef_list_elem) =
    Obj.magic _menhir_stack
  in
  ( assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : 'freshtv201 * _menhir_state * 'tv_typedef_list_elem) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_env = _menhir_discard _menhir_env in
          let _tok = _menhir_env._menhir_token in
          match _tok with
          | NAME _v ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                MenhirState21
          : 'freshtv202 )
    | FSTOP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : 'freshtv203 * _menhir_state * 'tv_typedef_list_elem) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, (_1 : 'tv_typedef_list_elem) =
            _menhir_stack
          in
          let _v : 'tv_typedef_list = [ _1 ] in
          _menhir_goto_typedef_list _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv204 )
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : 'freshtv205 * _menhir_state * 'tv_typedef_list_elem) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
          : 'freshtv206 )
    : 'freshtv208 )

and _menhir_goto_arith_base :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arith_base -> 'ttv_return
    =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let (_v : 'tv_arith_base) = _v in
  ( let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv197) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_arith_base) : 'tv_arith_base) = _v in
    ( let _v : 'tv_arith = Base _1 in
      _menhir_goto_arith _menhir_env _menhir_stack _menhir_s _v
      : 'freshtv198 )
    : 'freshtv200 )

and _menhir_goto_clause_body :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_clause_body -> 'ttv_return
    =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState60 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv173 * _menhir_state * 'tv_clause_body_one)
              * _menhir_state
              * 'tv_clause_body) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack
              : ('freshtv171 * _menhir_state * 'tv_clause_body_one)
                * _menhir_state
                * 'tv_clause_body) =
          Obj.magic _menhir_stack
        in
        ( let ( (_menhir_stack, _menhir_s, (_1 : 'tv_clause_body_one)),
                _,
                (_3 : 'tv_clause_body) ) =
            _menhir_stack
          in
          let _2 = () in
          let _v : 'tv_clause_body = _1 :: _3 in
          _menhir_goto_clause_body _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv172 )
        : 'freshtv174 )
  | MenhirState82 | MenhirState29 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv187 * _menhir_state * 'tv_clause_body) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_clause_body) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, (_1 : 'tv_clause_body) =
            _menhir_stack
          in
          let _v : 'tv_resolvant = Resolvant _1 in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv183) = _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (_v : 'tv_resolvant) = _v in
          ( match _menhir_s with
            | MenhirState29 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv177 * _menhir_state) =
                  Obj.magic _menhir_stack
                in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_resolvant) = _v in
                ( let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : 'freshtv175 * _menhir_state) =
                    Obj.magic _menhir_stack
                  in
                  let (_ : _menhir_state) = _menhir_s in
                  let ((_2 : 'tv_resolvant) : 'tv_resolvant) = _v in
                  ( let _menhir_stack, _menhir_s = _menhir_stack in
                    let _1 = () in
                    let _v : 'tv_program = Program (Sentence [], _2) in
                    _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
                    : 'freshtv176 )
                  : 'freshtv178 )
            | MenhirState82 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack
                      : 'freshtv181 * _menhir_state * 'tv_sentence) =
                  Obj.magic _menhir_stack
                in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_resolvant) = _v in
                ( let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack
                        : 'freshtv179 * _menhir_state * 'tv_sentence) =
                    Obj.magic _menhir_stack
                  in
                  let (_ : _menhir_state) = _menhir_s in
                  let ((_3 : 'tv_resolvant) : 'tv_resolvant) = _v in
                  ( let _menhir_stack, _menhir_s, (_1 : 'tv_sentence) =
                      _menhir_stack
                    in
                    let _2 = () in
                    let _v : 'tv_program = Program (_1, _3) in
                    _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
                    : 'freshtv180 )
                  : 'freshtv182 )
            | _ -> _menhir_fail ()
            : 'freshtv184 )
          : 'freshtv186 )
        : 'freshtv188 )
  | MenhirState78 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : ('freshtv195 * _menhir_state * 'tv_atom)
              * _menhir_state
              * 'tv_clause_body) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv191 * _menhir_state * 'tv_atom)
                    * _menhir_state
                    * 'tv_clause_body) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack
                    : ('freshtv189 * _menhir_state * 'tv_atom)
                      * _menhir_state
                      * 'tv_clause_body) =
                Obj.magic _menhir_stack
              in
              ( let ( (_menhir_stack, _menhir_s, (_1 : 'tv_atom)),
                      _,
                      (_3 : 'tv_clause_body) ) =
                  _menhir_stack
                in
                let _4 = () in
                let _2 = () in
                let _v : 'tv_clause = Clause (_1, _3) in
                _menhir_goto_clause _menhir_env _menhir_stack _menhir_s _v
                : 'freshtv190 )
              : 'freshtv192 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack
                  : ('freshtv193 * _menhir_state * 'tv_atom)
                    * _menhir_state
                    * 'tv_clause_body) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv194 )
        : 'freshtv196 )
  | _ -> _menhir_fail ()

and _menhir_goto_typedef_body :
    _menhir_env ->
    'ttv_tail ->
    _menhir_state ->
    'tv_typedef_body ->
    'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv169 * _menhir_state * 'tv_typedef_body) =
    Obj.magic _menhir_stack
  in
  ( assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_typedef_body) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_env = _menhir_discard _menhir_env in
          let _tok = _menhir_env._menhir_token in
          match _tok with
          | INTTYP ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
          | NAME _v ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                _v
          | VAR _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                MenhirState16
          : 'freshtv164 )
    | RBRAC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state * 'tv_typedef_body) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, (_1 : 'tv_typedef_body) =
            _menhir_stack
          in
          let _v : 'tv_typedef_body_list = [ _1 ] in
          _menhir_goto_typedef_body_list _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv166 )
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_typedef_body) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
          : 'freshtv168 )
    : 'freshtv170 )

and _menhir_goto_sentence_item :
    _menhir_env ->
    'ttv_tail ->
    _menhir_state ->
    'tv_sentence_item ->
    'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_sentence_item) =
    Obj.magic _menhir_stack
  in
  ( assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NAME _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | PRED -> _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TYPEDEF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | END | QUES ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_sentence_item) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, (_1 : 'tv_sentence_item) =
            _menhir_stack
          in
          let _v : 'tv_sentence = Sentence [ _1 ] in
          _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv160 )
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71
    : 'freshtv162 )

and _menhir_fail : unit -> 'a =
 fun () ->
  Printf.fprintf Pervasives.stderr
    "Internal failure -- please contact the parser generator's developers.\n%!";
  assert false

and _menhir_goto_clause :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_clause -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let (_v : 'tv_clause) = _v in
  ( let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_clause) : 'tv_clause) = _v in
    ( let _v : 'tv_sentence_item = C _1 in
      _menhir_goto_sentence_item _menhir_env _menhir_stack _menhir_s _v
      : 'freshtv156 )
    : 'freshtv158 )

and _menhir_goto_term :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_term) =
    Obj.magic _menhir_stack
  in
  ( assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_term) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_env = _menhir_discard _menhir_env in
          let _tok = _menhir_env._menhir_token in
          match _tok with
          | INT _v ->
              _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                _v
          | NAME _v ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                _v
          | VAR _v ->
              _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                MenhirState51
          : 'freshtv148 )
    | RBRAC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_term) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, (_1 : 'tv_term) = _menhir_stack in
          let _v : 'tv_term_list = [ _1 ] in
          _menhir_goto_term_list _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv150 )
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_term) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
          : 'freshtv152 )
    : 'freshtv154 )

and _menhir_run3 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | COMMA ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv141 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | VAR _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
              MenhirState4
        : 'freshtv142 )
  | EQUALS ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv143 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, (_1 : string) = _menhir_stack in
        let _v : 'tv_var_list = [ _1 ] in
        _menhir_goto_var_list _menhir_env _menhir_stack _menhir_s _v
        : 'freshtv144 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv145 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv146 )

and _menhir_run7 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | LBRAC ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv135 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INTTYP ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | NAME _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | VAR _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
              MenhirState8
        : 'freshtv136 )
  | COMMA | FSTOP ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv137 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, (_1 : string) = _menhir_stack in
        let _v : 'tv_typedef_list_elem = TypeDefRight (_1, []) in
        _menhir_goto_typedef_list_elem _menhir_env _menhir_stack _menhir_s _v
        : 'freshtv138 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv139 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv140 )

and _menhir_run32 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_env = _menhir_discard _menhir_env in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let ((_1 : string) : string) = _v in
  ( let _v : 'tv_arith_base = Var _1 in
    _menhir_goto_arith_base _menhir_env _menhir_stack _menhir_s _v
    : 'freshtv134 )

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_stack = (_menhir_stack, _menhir_s) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | INT _v ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv129 * _menhir_state) =
        Obj.magic _menhir_stack
      in
      let (_v : int) = _v in
      ( let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state) =
          Obj.magic _menhir_stack
        in
        let ((_2 : int) : int) = _v in
        ( let _menhir_stack, _menhir_s = _menhir_stack in
          let _1 = () in
          let _v : 'tv_arith_base = Int (_2 * -1) in
          _menhir_goto_arith_base _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv128 )
        : 'freshtv130 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv131 * _menhir_state) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv132 )

and _menhir_run35 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> int -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_env = _menhir_discard _menhir_env in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let ((_1 : int) : int) = _v in
  ( let _v : 'tv_arith_base = Int _1 in
    _menhir_goto_arith_base _menhir_env _menhir_stack _menhir_s _v
    : 'freshtv126 )

and _menhir_goto_clause_body_one :
    _menhir_env ->
    'ttv_tail ->
    _menhir_state ->
    'tv_clause_body_one ->
    'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_clause_body_one) =
    Obj.magic _menhir_stack
  in
  ( assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * 'tv_clause_body_one)
            =
          Obj.magic _menhir_stack
        in
        ( let _menhir_env = _menhir_discard _menhir_env in
          let _tok = _menhir_env._menhir_token in
          match _tok with
          | CUT ->
              _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | FAIL ->
              _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | NAME _v ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                _v
          | VAR _v ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                MenhirState60
          : 'freshtv118 )
    | END | FSTOP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_clause_body_one)
            =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, (_1 : 'tv_clause_body_one) =
            _menhir_stack
          in
          let _v : 'tv_clause_body = [ _1 ] in
          _menhir_goto_clause_body _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv120 )
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_clause_body_one)
            =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
          : 'freshtv122 )
    : 'freshtv124 )

and _menhir_run9 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_env = _menhir_discard _menhir_env in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let ((_1 : string) : string) = _v in
  ( let _v : 'tv_typedef_body = TypeVar _1 in
    _menhir_goto_typedef_body _menhir_env _menhir_stack _menhir_s _v
    : 'freshtv116 )

and _menhir_run10 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | LBRAC ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv109 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INTTYP ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NAME _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
              _v
        | VAR _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
              MenhirState11
        : 'freshtv110 )
  | COMMA | RBRAC ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv111 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, (_1 : string) = _menhir_stack in
        let _v : 'tv_typedef_body = TypeCons (_1, []) in
        _menhir_goto_typedef_body _menhir_env _menhir_stack _menhir_s _v
        : 'freshtv112 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv113 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv114 )

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_env = _menhir_discard _menhir_env in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  ( let _1 = () in
    let _v : 'tv_typedef_body = IntTyp in
    _menhir_goto_typedef_body _menhir_env _menhir_stack _menhir_s _v
    : 'freshtv108 )

and _menhir_goto_pred_def :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_pred_def -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv105) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let (_v : 'tv_pred_def) = _v in
  ( let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv103) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_pred_def) : 'tv_pred_def) = _v in
    ( let _v : 'tv_sentence_item = P _1 in
      _menhir_goto_sentence_item _menhir_env _menhir_stack _menhir_s _v
      : 'freshtv104 )
    : 'freshtv106 )

and _menhir_goto_atom :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atom -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState82 | MenhirState78 | MenhirState29 | MenhirState60 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv91 * _menhir_state * 'tv_atom) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * 'tv_atom) =
          Obj.magic _menhir_stack
        in
        ( let _menhir_stack, _menhir_s, (_1 : 'tv_atom) = _menhir_stack in
          let _v : 'tv_clause_body_one = CAT _1 in
          _menhir_goto_clause_body_one _menhir_env _menhir_stack _menhir_s _v
          : 'freshtv90 )
        : 'freshtv92 )
  | MenhirState0 | MenhirState71 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv101 * _menhir_state * 'tv_atom) =
        Obj.magic _menhir_stack
      in
      ( assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv93 * _menhir_state * 'tv_atom) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              match _tok with
              | CUT ->
                  _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState78
              | FAIL ->
                  _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState78
              | NAME _v ->
                  _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState78 _v
              | VAR _v ->
                  _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState78 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                    MenhirState78
              : 'freshtv94 )
        | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv97 * _menhir_state * 'tv_atom) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv95 * _menhir_state * 'tv_atom) =
                Obj.magic _menhir_stack
              in
              ( let _menhir_stack, _menhir_s, (_1 : 'tv_atom) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_clause = Clause (_1, []) in
                _menhir_goto_clause _menhir_env _menhir_stack _menhir_s _v
                : 'freshtv96 )
              : 'freshtv98 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv99 * _menhir_state * 'tv_atom) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv100 )
        : 'freshtv102 )
  | _ -> _menhir_fail ()

and _menhir_run44 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_env = _menhir_discard _menhir_env in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv87) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let ((_1 : string) : string) = _v in
  ( let _v : 'tv_term = TVar _1 in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
    : 'freshtv88 )

and _menhir_run45 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | LBRAC ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv81 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
              _v
        | NAME _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46
              _v
        | VAR _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46
              _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
              MenhirState46
        : 'freshtv82 )
  | COMMA | RBRAC ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv83 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, (_1 : string) = _menhir_stack in
        let _v : 'tv_term = TFun (_1, []) in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
        : 'freshtv84 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv85 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv86 )

and _menhir_run47 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> int -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_env = _menhir_discard _menhir_env in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv79) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  let ((_1 : int) : int) = _v in
  ( let _v : 'tv_term = TInt _1 in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
    : 'freshtv80 )

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_stack = (_menhir_stack, _menhir_s) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | NAME _v ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv75 * _menhir_state) =
        Obj.magic _menhir_stack
      in
      let (_v : string) = _v in
      ( let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv73 * _menhir_state) * string) =
              Obj.magic _menhir_stack
            in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ( let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              match _tok with
              | NAME _v ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState6 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                    MenhirState6
              : 'freshtv74 )
        | VAR _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
              MenhirState2
        : 'freshtv76 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv77 * _menhir_state) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv78 )

and _menhir_errorcase :
    _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s ->
  match _menhir_s with
  | MenhirState82 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_sentence) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv32 )
  | MenhirState78 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_atom) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv34 )
  | MenhirState71 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_sentence_item) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv36 )
  | MenhirState66 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : ('freshtv37 * _menhir_state) * string) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_stack, _menhir_s), _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv38 )
  | MenhirState60 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_clause_body_one) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv40 )
  | MenhirState51 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv41 * _menhir_state * 'tv_term) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv42 )
  | MenhirState46 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv43 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv44 )
  | MenhirState43 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv45 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv46 )
  | MenhirState40 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv47 * _menhir_state * 'tv_arith) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv48 )
  | MenhirState38 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_arith) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv50 )
  | MenhirState31 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv51 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv52 )
  | MenhirState29 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv53 * _menhir_state) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv54 )
  | MenhirState26 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : (('freshtv55 * _menhir_state) * string)
              * _menhir_state
              * 'tv_var_list) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv56 )
  | MenhirState21 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_typedef_list_elem)
          =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv58 )
  | MenhirState16 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_typedef_body) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv60 )
  | MenhirState11 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv61 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv62 )
  | MenhirState8 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv63 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv64 )
  | MenhirState6 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack
            : (('freshtv65 * _menhir_state) * string) * _menhir_state) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv66 )
  | MenhirState4 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv67 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv68 )
  | MenhirState2 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : ('freshtv69 * _menhir_state) * string) =
        Obj.magic _menhir_stack
      in
      ( let (_menhir_stack, _menhir_s), _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv70 )
  | MenhirState0 ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv71) = Obj.magic _menhir_stack in
      (raise _eRR : 'freshtv72)

and _menhir_run30 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | IS ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv27 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31
              _v
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | VAR _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
              _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
              MenhirState31
        : 'freshtv28 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv29 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv30 )

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_env = _menhir_discard _menhir_env in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv25) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  ( let _1 = () in
    let _v : 'tv_clause_body_one = Fail in
    _menhir_goto_clause_body_one _menhir_env _menhir_stack _menhir_s _v
    : 'freshtv26 )

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_env = _menhir_discard _menhir_env in
  let (_menhir_env : _menhir_env) = _menhir_env in
  let (_menhir_stack : 'freshtv23) = Obj.magic _menhir_stack in
  let (_menhir_s : _menhir_state) = _menhir_s in
  ( let _1 = () in
    let _v : 'tv_clause_body_one = Cut in
    _menhir_goto_clause_body_one _menhir_env _menhir_stack _menhir_s _v
    : 'freshtv24 )

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_stack = (_menhir_stack, _menhir_s) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | NAME _v ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv19 * _menhir_state) =
        Obj.magic _menhir_stack
      in
      let (_v : string) = _v in
      ( let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FSTOP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * string) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv11 * _menhir_state) * string) =
                Obj.magic _menhir_stack
              in
              ( let (_menhir_stack, _menhir_s), (_2 : string) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_pred_def = PredDef (_2, []) in
                _menhir_goto_pred_def _menhir_env _menhir_stack _menhir_s _v
                : 'freshtv12 )
              : 'freshtv14 )
        | LBRAC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv15 * _menhir_state) * string) =
              Obj.magic _menhir_stack
            in
            ( let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              match _tok with
              | INTTYP ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState66
              | NAME _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState66 _v
              | VAR _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
                    MenhirState66 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                    MenhirState66
              : 'freshtv16 )
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv17 * _menhir_state) * string) =
              Obj.magic _menhir_stack
            in
            ( let (_menhir_stack, _menhir_s), _ = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
              : 'freshtv18 )
        : 'freshtv20 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv21 * _menhir_state) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv22 )

and _menhir_run42 :
    _menhir_env -> 'ttv_tail -> _menhir_state -> string -> 'ttv_return =
 fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | LBRAC ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv5 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState43
              _v
        | NAME _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState43
              _v
        | VAR _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
              _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
              MenhirState43
        : 'freshtv6 )
  | ARR | COMMA | END | FSTOP ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv7 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, (_1 : string) = _menhir_stack in
        let _v : 'tv_atom = Atom (_1, []) in
        _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v
        : 'freshtv8 )
  | _ ->
      assert (not _menhir_env._menhir_error);
      _menhir_env._menhir_error <- true;
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv9 * _menhir_state * string) =
        Obj.magic _menhir_stack
      in
      ( let _menhir_stack, _menhir_s, _ = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
        : 'freshtv10 )

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

and main :
    (Lexing.lexbuf -> token) -> Lexing.lexbuf -> string ParseTree.program =
 fun lexer lexbuf ->
  let _menhir_env =
    let (lexer : Lexing.lexbuf -> token) = lexer in
    let (lexbuf : Lexing.lexbuf) = lexbuf in
    ( let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }
      : _menhir_env )
  in
  Obj.magic
    (let (_menhir_env : _menhir_env) = _menhir_env in
     let (_menhir_stack : 'freshtv3) =
       ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p)
     in
     ( let _menhir_env = _menhir_discard _menhir_env in
       let _tok = _menhir_env._menhir_token in
       match _tok with
       | NAME _v ->
           _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
       | PRED ->
           _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState0
       | QUES ->
           let (_menhir_env : _menhir_env) = _menhir_env in
           let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
           let (_menhir_s : _menhir_state) = MenhirState0 in
           ( let _menhir_stack = (_menhir_stack, _menhir_s) in
             let _menhir_env = _menhir_discard _menhir_env in
             let _tok = _menhir_env._menhir_token in
             match _tok with
             | CUT ->
                 _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
                   MenhirState29
             | FAIL ->
                 _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
                   MenhirState29
             | NAME _v ->
                 _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
                   MenhirState29 _v
             | VAR _v ->
                 _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
                   MenhirState29 _v
             | _ ->
                 assert (not _menhir_env._menhir_error);
                 _menhir_env._menhir_error <- true;
                 _menhir_errorcase _menhir_env (Obj.magic _menhir_stack)
                   MenhirState29
             : 'freshtv2 )
       | TYPEDEF ->
           _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
       | _ ->
           assert (not _menhir_env._menhir_error);
           _menhir_env._menhir_error <- true;
           _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0
       : 'freshtv4 ))
