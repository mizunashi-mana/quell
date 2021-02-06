Syntax
======

Notational Conventions
----------------------

.. glossary::

    ``pattern?``
        optional

    ``pattern*``
        zero or more repetitions

    ``pattern+``
        one or more repetitions

    ``( pattern )``
        grouping

    ``pattern | pattern``
        choice

    ``pattern<pattern>``
        difference

    ``"..."``
        terminal by unicode properties

    ``'...'``
        virtual layout terminal (See `Layout`_)

    ``EOS``
        end of source

Lexical Syntax
--------------

.. productionlist::
    lexical_program: (lexeme | whitespace)* EOS?
    lexeme  : literal
            : special
            : brace
            : reserved_id
            : reserved_op
            : var_id
            : var_op
            : con_id
            : con_op

.. productionlist::
    var_id: (small (small | large | digit | other)*)<reserved_id>
    con_id: (large (small | large | digit | other)*)<reserved_id>
    var_op: (symbol<":"> (symbol | other)*)<reserved_op>
    con_op: (":" (symbol | other)*)<reserved_op>

.. productionlist::
    reserved_id : "as"
                : "case"
                : "data"
                : "derive"
                : "do"
                : "export"
                : "family"
                : "foreign"
                : "impl"
                : "infix"
                : "letrec"
                : "let"
                : "module"
                : "newtype"
                : "of"
                : "pattern"
                : "record"
                : "role"
                : "signature"
                : "static"
                : "trait"
                : "type"
                : "use"
                : "when"
                : "where"
                : "_"
                : "Default"
                : "Self"
    reserved_op : "!"
                : "->" | "→"
                : ".." | "…"
                : "."
                : "<-" | "←"
                : "<=" | "⇐"
                : "=>" | "⇒"
                : "="
                : "?"
                : "@"
                : "\\/" | "∀"
                : "\\" | "λ"
                : "|"
                : "~"
                : "::"
                : ":"
    special : "("
            : ")"
            : ","
            : "["
            : "]"
            : "`" -- `
            : ";"
    brace   : "{{" | "}}" : "❴" | "❵"
            : "{" | "}"

.. productionlist::
    literal : rational
            : integer
            : bytestring
            : string
            : bytechar
            : char
            : interp_string_part

.. productionlist::
    rational: sign? decimal "." decimal exponent?
            : sign? decimal exponent
    integer : sign? zero ("b" | "B") bit (bit | "_")*
            : sign? zero ("o" | "O") octit (octit | "_")*
            : sign? zero ("x" | "X") hexit (hexit | "_")*
            : sign? decimal
    decimal: digit (digit | "_")*
    sign: "+"
        : "-"
    zero: "0"
    exponent: ("e" | "E") sign? decimal
    bit: "0" | "1"
    octit: "0" | "1" | ... | "7"
    hexit   : digit
            : "A" | "B" | ... | "F"
            : "a" | "b" | ... | "f"

.. productionlist::
    bytestring: split_open "r" str_sep bstr_graphic* str_sep
    string: str_sep (bstr_graphic | uni_escape)* str_sep
    bytechar: split_open "r" char_sep bchar_graphic char_sep
    char: char_sep (bchar_graphic | uni_escape) char_sep
    split_open: "#"
    str_sep: "\""
    char_sep: "'"
    escape_open: "\\"
    bstr_graphic: graphic<str_sep | escape_open>
                : whitechar
                : byte_escape
                : gap
    bchar_graphic   : graphic<char_sep | escape_open>
                    : " "
                    : byte_escape
    byte_escape: escape_open (charesc | asciiesc | byteesc)
    uni_escape: escape_open "u{" hexit+ "}"
    gap: escape_open "|" whitechar* "|"
    charesc : "0" | "a" | "b" | "f" | "n" | "r" | "t" | "v"
            : "$" | escape_open | str_sep | char_sep
    asciiesc: "^" cntrlesc
            : "NUL" | "SOH" | "STX" | "ETX" | "EOT" | "ENQ"
            : "ACK" | "BEL" | "BS" | "HT" | "LF" | "VT"
            : "FF" | "CR" | "SO" | "SI" | "DLE" | "DC1"
            : "DC2" | "DC3" | "DC4" | "NAK" | "SYN" | "ETB"
            : "CAN" | "EM" | "SUB" | "ESC" | "FS" | "GS"
            : "RS" | "US" | "SP" | "DEL"
    cntrlesc: "A" | "B" | ... | "Z" | "@" | "[" | "\\" | "]"
            : "^" | "_"
    byteesc: "x" hexit hexit

.. productionlist::
    interp_string_part  : interp_string_without_interp
                        : interp_string_start
                        : interp_string_cont
                        : interp_string_end
    interp_str_open: split_open "s" str_sep
    interp_str_graphic  : bstr_graphic<"$" | str_sep | escape_open>
                        : uni_escape
    interp_open: "$" ( "{#" | "⦃" )
    interp_close: "#}" | "⦄"
    interp_string_without_interp: interp_str_open interp_str_graphic* str_sep
    interp_string_start: interp_str_open interp_str_graphic* interp_open
    interp_string_cont: interp_close interp_str_graphic* interp_open
    interp_string_end: interp_close interp_str_graphic* str_sep

.. productionlist::
    whitespace: whitestuff+
    whitestuff  : whitechar
                : comment

.. productionlist::
    comment : line_comment
            : doc_comment
            : pragma_comment
            : multiline_comment
    line_comment: "--" "-"* (any<symbol | other> any*)? (newline | EOS)
    multiline_comment: comment_open (ANY<"!" | "#"> ANYs (nested_comment ANYs)*)? comment_close
    doc_comment: comment_open "!" (ANY*)<ANY* newline "|" comment_close ANY*> newline "|" comment_close
    pragma_comment: comment_open "#" ANYs (nested_comment ANYs)* "#" comment_close
    nested_comment: comment_open ANYs (nested_comment ANYs)* comment_close
    comment_open: "{-"
    comment_close: "-}"
    any: graphic | space
    ANYs: (ANY*)<ANY* (comment_open | comment_close) ANY*>
    ANY: graphic | whitechar

.. productionlist::
    graphic : small
            : large
            : symbol
            : digit
            : other
            : special
            : other_special
            : other_graphic
    whitechar   : "\v"
                : space
                : newline
    space   : "\t" | "\u200E" | "\u200F"
            : "\p{General_Category=Space_Separator}"
    newline : "\r\n" | "\r" | "\n" | "\f"
            : "\p{General_Category=Line_Separator}"
            : "\p{General_Category=Paragraph_Separator}"
    small   : "\p{General_Category=Lowercase_Letter}"
            : "\p{General_Category=Other_Letter}"
            : "_"
    large   : "\p{General_Category=Uppercase_Letter}"
            : "\p{General_Category=Titlecase_Letter}"
    symbol  : symbolchar<special | other_special | "_" | "'">
    symbolchar  : "\p{General_Category=Connector_Punctuation}"
                : "\p{General_Category=Dash_Punctuation}"
                : "\p{General_Category=Other_Punctuation}"
                : "\p{General_Category=Symbol}"
    digit   : "\p{General_Category=Decimal_Number}"
    other   : "\p{General_Category=Modifier_Letter}"
            : "\p{General_Category=Mark}"
            : "\p{General_Category=Letter_Number}"
            : "\p{General_Category=Other_Number}"
            : "\p{General_Category=Format}"<whitechar>
            : "'"
    other_special: "#" | "\"" | "{" | "}" | "⦃" | "⦄" | "❴" | "❵"
    other_graphic: other_graphic_char<symbolchar | special | other_special>
    other_graphic_char: "\p{General_Category=Punctuation}"

Specifications for Lexical Nonterminals
:::::::::::::::::::::::::::::::::::::::

These nonterminals must be disjoint:

* ``whitespace``
* ``var_id``
* ``var_op``
* ``con_id``
* ``con_op``
* ``reserved_id``
* ``reserved_op``
* ``special``
* ``brace``
* ``literal``

These nonterminals must be disjoint:

* ``whitechar``
* ``small``
* ``large``
* ``symbol``
* ``digit``
* ``other``
* ``special``
* ``other_special``
* ``other_graphic``

These nonterminals must be disjoint:

* ``space``
* ``newline``

These expressions must be empty:

* ``((lexeme | whitespace)*)<ANY*>``
* ``reserved_id<(small | large) (small | large | digit | other)*>``
* ``reserved_op<symbol (symbol | other)*>``
* ``(brace)<other_special*>``
* ``literal<("+" | "-" | digit | "'" | other_special) ANY*>``
* ``(multiline_comment | doc_comment | pragma_comment | nested_comment)<comment_open ANY* comment_close>``
* ``(multiline_comment | doc_comment | pragma_comment)<doc_comment | nested_comment>``
* ``("\p{General_Category=Letter}" | "\p{General_Category=Mark}" | "\p{General_Category=Number}" | "\p{General_Category=Punctuation}" | "\p{General_Category=Symbol}" | "\p{General_Category=Separator}" | "\p{General_Category=Format}")<graphic | whitechar>``

Aliases
-------

.. productionlist::
    "->"    : "->" | "→"
    ".."    : ".." | "…"
    "<-"    : "<-" | "←"
    "<="    : "<=" | "⇐"
    "=>"    : "=>" | "⇒"
    "\\/"   : "\\/" | "∀"
    "\\"    : "\\" | "λ"
    "{{"    : "{{" | "❴"
    "}}"    : "}}" | "❵"

Grammar
-------

.. productionlist::
    program: module_decl_body

.. productionlist::
    module_decl: "module" declconexpr ("where" module_decl_body)?
    module_decl_body: lopen module_decl_items lclose
    module_decl_items: (module_decl_item lsemis)* module_decl_item?
    module_decl_item: sig_item
                    : type_decl
                    : type_family_decl
                    : type_impl_decl
                    : data_decl
                    : val_decl
                    : module_decl
                    : pattern_decl
                    : trait_decl
                    : impl_decl
                    : fixity_decl
                    : foreign_val_decl
                    : export_clause
                    : derive_clause

.. productionlist::
    typesig_decl: "type" con ":" type
    valsig_decl: var ":" type
    consig_decl: con ":" type
    patternsig_decl: "pattern" con ":" type
    foreign_val_decl: "foreign" string var ":" type

.. productionlist::
    type_decl: "type" decltype "=" type ("where" type_decl_where_body)?
    type_decl_where_body : lopen type_decl_where_items lclose
    type_decl_where_items: (type_decl_where_item lsemis)* type_decl_where_item?
    type_decl_where_item: type_decl
                        : use_clause

.. productionlist::
    type_family_decl: "type" "family" con (":" type)? ("where" ctypefam_decl_body)?
                    : "data" "family" con (":" type)? ("where" cdatafam_decl_body)?
    ctypefam_decl_body  : lopen ctypefam_decl_items lclose
    ctypefam_decl_items: (ctypefam_decl_item lsemis)* ctypefam_decl_item?
    ctypefam_decl_item  : typefam_impl_decl
                        : type_decl_where_item
    cdatafam_decl_body  : lopen cdatafam_decl_items lclose
    cdatafam_decl_items: (cdatafam_decl_item lsemis)* cdatafam_decl_item?
    cdatafam_decl_item  : datafam_impl_decl
                        : type_decl_where_item

.. productionlist::
    type_impl_decl  : typefam_impl_decl
                    : datafam_impl_decl
    typefam_impl_decl: "type" "impl" impltype "=" type ("where" type_decl_where_body)?
    datafam_impl_decl   : "data" "impl" impltype ("where" data_decl_body)?
                        : "newtype" "impl" impltype "=" type ("where" type_decl_where_body)?

.. productionlist::
    data_decl   : "data" con (":" type)? ("where" data_decl_body)?
                : "newtype" decltype "=" type ("where" type_decl_where_body)?
    data_decl_body  : lopen data_decl_items lclose
    data_decl_items: (data_decl_item lsemis)* data_decl_item?
    data_decl_item: consig_decl

.. productionlist::
    val_decl: declvarexpr "=" expr ("where" val_decl_where_body)?
    val_bind: pat "=" expr ("where" val_decl_where_body)?
    val_decl_where_body : lopen val_decl_where_items lclose
    val_decl_where_items: (val_decl_where_item lsemis)* val_decl_where_item?
    val_decl_where_item: let_bind_item

.. productionlist::
    pattern_decl: "pattern" "_" (":" type)? "of" pattern_decl_body
                : "pattern" declpat "=" pat
                : "pattern" declpat "<-" pat
    pattern_decl_body   : lopen pattern_decl_items lclose
    pattern_decl_items: (pattern_decl_item lsemis)* pattern_decl_item?
    pattern_decl_item   : declpat "=" pat
                        : declpat "<-" pat

.. productionlist::
    trait_decl: "trait" decltype ("<=" context)* ("where" trait_decl_body)?
    trait_decl_body : lopen trait_decl_items lclose
    trait_decl_items: (trait_decl_item lsemis)* trait_decl_item?
    trait_decl_item : sig_item
                    : fixity_decl

.. productionlist::
    impl_decl: "impl" impltype ("<=" context)* ("as" con)? ("where" impl_decl_body)?
    impl_decl_body  : lopen impl_decl_items lclose
    impl_decl_items: (impl_decl_item lsemis)* impl_decl_item?
    impl_decl_item: module_decl_item

.. productionlist::
    fixity_decl: "infix" infix_assoc? infix_prec (op ",")* op ","?
    infix_assoc: "<-" | "->"
    infix_prec: integer

.. productionlist::
    use_clause: "use" (string ":")?  (con ".")* use_body
    use_body    : "{" ".." "}"
                : "{" (use_item ",")* use_item? "}"
                : use_item
    use_item    : con_id_ext ("as" con)?
                : con_sym_ext ("as" conop)?
                : var_id_ext ("as" var)?
                : var_sym_ext ("as" op)?

.. productionlist::
    export_clause: "export" export_body
    export_body : "(" (export_item ",")* export_item? ")"
                : export_item
    export_item : con ("as" con)?
                : conop ("as" conop)?
                : var ("as" var)?
                : op ("as" op)?

.. productionlist::
    derive_clause: "derive" impltype ("<=" context)* ("as" con)?

.. productionlist::
    decltype    : declcon bind_var*
                : bind_var declconop bind_var
    impltype    : con type_qualified*
                : type_qualified conop type_qualified
    declconexpr : declcon bind_var*
                : bind_var declconop bind_var
    declvarexpr : declvar bind_var*
                : bind_var declop bind_var
    declpat     : declcon bind_var*
                : bind_var declconop bind_var

.. productionlist::
    type: "\\/" bind_var* "=>" type
        : context "=>" type
        : type_expr
    context: type_unit
    type_expr   : type_unit "->" type
                : type_unit
    type_unit: type_infix
    type_infix: type_apps (type_op type_apps)*
    type_op : con_sym
            : var_sym_ext
            : "`" type_qualified_op "`"
    type_qualified_op   : (type_atomic ".")* sym_ext
                        : type_qualified
    type_apps: type_qualified type_app*
    type_app: "@" type_qualified
            : type_qualified
    type_qualified: (type_atomic ".")* type_atomic
    type_atomic : "(" type (":" type)? ")"
                : con
                : var
                : type_literal
    type_literal: literal
                : "(" type_tuple_items ")"
                : "[" type_array_items "]"
                : "{" type_simplrecord_items "}"
                : "record" type_record_body
                : "signature" sig_body
    type_tuple_items: (type ",")+ type ","?
    type_array_items: (type ",")* type?
    type_simplrecord_items: (type_simplrecord_item ",")* type_simplrecord_item?
    type_simplrecord_item: var ":" type
    type_record_body: lopen type_record_items lclose
    type_record_items: (type_record_item lsemis)* type_record_item?
    type_record_item: valsig_decl
    sig_body: lopen sig_items lclose
    sig_items: (sig_item lsemis)* sig_item?
    sig_item: typesig_decl
            : valsig_decl
            : consig_decl
            : patternsig_decl
            : use_clause

.. productionlist::
    expr: expr_infix ":" type
        : expr_infix
    expr_infix: expr_apps ((qual_op | qual_conop) expr_apps)*
    expr_apps: expr_qualified expr_app*
    expr_app: expr_qualified
            : "@" type_qualified
    expr_qualified: (expr_block ".")* expr_block
    expr_block  : "\\" "case" case_alt_body
                : "\\" lambda_body
                : "letrec" let_body
                : "let" let_body
                : "case" (expr ",")* expr? "of" case_alt_body
                : "do" do_body
                : expr_atomic
    expr_atomic : "(" expr ")"
                : con
                : var
                : expr_literal
    expr_literal: literal
                : expr_interp_string
                : "(" expr_tuple_items ")"
                : "[" expr_array_items "]"
                : "{" expr_simplrecord_items "}"
                : "record" expr_record_body
    expr_interp_string  : interp_string_without_interp
                        : interp_string_start expr (interp_string_cont expr)* interp_string_end
    expr_tuple_items: (expr ",")+ expr ","?
    expr_array_items: (expr ",")* expr?
    expr_simplrecord_items: (expr_simplrecord_item ",")* expr_simplrecord_item?
    expr_simplrecord_item: var "=" expr
    expr_record_body: lopen expr_record_items lclose
    expr_record_items: (expr_record_item lsemis)* expr_record_item?
    expr_record_item: valsig_decl
                    : val_decl

.. productionlist::
    pat : pat_unit ":" type
        : pat_unit
    pat_unit: pat_infix ("|" pat_infix)*
    pat_infix: pat_apps (qual_conop  pat_apps)*
    pat_apps: pat_qualified pat_app*
    pat_app : pat_qualified
            : "@" pat_qualified
    pat_qualified: (con ".")* pat_atomic
    pat_atomic  : "(" pat ")"
                : con
                : var
                : pat_literal
    pat_literal : literal
                : "(" pat_tuple_items ")"
                : "[" pat_array_items "]"
                : "{" pat_simplrecord_items "}"
    pat_tuple_items: (pat ",")+ pat ","?
    pat_array_items: (pat ",")* pat?
    pat_simplrecord_items: (pat_simplrecord_item ",")* pat_simplrecord_item?
    pat_simplrecord_item: var "=" pat

.. productionlist::
    let_body: let_binds "in" expr
    let_binds   : lopen let_bind_items lclose
    let_bind_items: (let_bind_item lsemis)* let_bind_item?
    let_bind_item   : sig_item
                    : type_decl
                    : type_family_decl
                    : type_impl_decl
                    : data_decl
                    : val_bind
                    : module_decl
                    : pattern_decl
                    : trait_decl
                    : impl_decl
                    : fixity_decl
                    : foreign_val_decl
                    : derive_clause

.. productionlist::
    case_alt_body: lopen case_alt_items lclose
    case_alt_items: (case_alt_item lsemis)* case_alt_item?
    case_alt_item: (pat ",")* pat? guarded_alt
    guarded_alt : "->" expr
                : "when" guarded_alt_body
    guarded_alt_body: lopen guarded_alt_items lclose
    guarded_alt_items: (guarded_alt_item lsemis)* guarded_alt_item?
    guarded_alt_item: guard_qual "->" expr
    guard_qual: expr

.. productionlist::
    lambda_body : pat_atomic* guarded_alt

.. productionlist::
    do_body : lopen do_stmt_items lclose
    do_stmt_items   : (do_stmt_item lsemis)* expr lsemis?
    do_stmt_item    : expr
                    : pat "<-" expr
                    : pat "=" expr
                    : ("let" | "letrec") let_binds

.. productionlist::
    bind_var: "@" simple_bind_var
            : simple_bind_var
    simple_bind_var : var_id_ext
                    : "(" var_id_ext ":" type ")"
    con : con_id_ext
        : "(" con_sym_ext ")"
    conop   : con_sym_ext
            : "`" con_id_ext "`"
    var : var_id_ext
        : "(" var_sym_ext ")"
    op  : var_sym_ext
        : "`" var_id_ext "`"
    sym_ext : con_sym_ext
            : var_sym_ext

.. productionlist::
    declcon : con_id
            : "(" con_sym ")"
    declconop   : con_sym
                : "`" con_id "`"
    con_id_ext  : con_id
                : "(" ")"
    con_sym_ext : con_sym
                : "->"
    declvar : con_id
            : "(" con_sym ")"
    declop  : con_sym
            : "`" con_id "`"
    var_id_ext  : var_id
                : "_"
    var_sym_ext : var_sym

.. productionlist::
    lopen: '{' lsemis?
    lclose: '}'
    lsemis: (';' | ";")+

Note:

* ``if`` 式はいれない．以下の標準関数で代用::

    if : \/a. Bool -> { then: a, else: a } -> a
    if = \with
        True,  e -> e.then
        False, e -> e.else

* multi way if は lambda case で代替::

    func1 : \/a. Int -> a -> Maybe a
    func1 = \with
        0, x -> Just x
        i, x when
            i > 10 -> Just x
            else   -> Nothing

    func2 : Int -> a -> Maybe a
    func2 = \i x -> \when
        i == 0 -> Just x
        i > 10 -> Just x
        else   -> Nothing

TODO:

* 不要な Keyword 取り除く & 適切な単語選ぶ
* レコード / モジュールの演算 (extend / union)
* レコード / モジュール部分多相
* モジュールの構文再定義 (ファイルシステムとのマッピング，可視性)
* プラグマの名前をちゃんと取るように
* ドキュメントコメントの lexical syntax 定義

Layout
------

* パーサは layout context を持つ

.. code-block:: haskell

    data TokenWithL
        = Token Int String
        | ExpectBrace
        | Newline Int

    preParse ts = go ts 0 isLayoutKeyword where
        go ts pl isL = skipWhiteSpace ts pl \(c,t) ts l -> case t of
            "\\" ->
                Token c t:go ts l isLayoutKeywordLam
            _ | isL t ->
                Token c t:ExpectBrace:go ts l isLayoutKeyword
            _ ->
                Token c t:go ts l isLayoutKeyword

    skipWhiteSpace ts pl cont = case ts of
        [] -> []
        (l,c,t):ts
            | isWhiteSpace t ->
                skipWhiteSpace ts pl cont
            | pl < l ->
                Newline c:cont (c,t) ts l
            | otherwise ->
                cont (c,t) ts l

    isWhiteSpace t =
        t match whitespace

    isLayoutKeyword t = case t of
        "when"      -> True
        "let"       -> True
        "letrec"    -> True
        "of"        -> True
        "when"      -> True
        _           -> False

    isLayoutKeywordLam t = case t of
        "case"  -> True
        _       -> isLayoutKeyword t

.. code-block:: haskell

    data Layout
        = NoLayout
        | ExplicitBrace
        | ExplicitDBrace Int
        | VirtualBrace Int

    parseWithL p ts = withL p ts []

    parseWithoutL p ts = case ts of
        [] ->
            ParseOk p
        Token _ t:ts -> parse p t \r -> case r of
            ParseOk p ->
                parseWithoutL p ts
            ParseError ->
                ParseError
        _:ts ->
            parseWithoutL p ts

    withL p ts ms = case ts of
        [] ->
            tryEnd p ms
        Token _ t:ts
            | isOpen t ->
                runParserL p t ts ms \p ts ms ->
                    withL p ts (NoLayout:ms)
            | isClose t || t match interp_string_continue ->
                tryClose p t ts ms
            | otherwise ->
                runParserL p t ts ms withL
        ExpectBrace:ts ->
            resolveBraceOpen p ts ms
        Newline n:ts ->
            resolveNewline n ts ms

    runParserL p t ts ms cont = parse p t \r -> case r of
        ParseOk p ->
            cont p ts ms
        ParseError ->
            errorRecover p t ts ms cont

    runSimpleParserL p t cont = parse p t \r -> case r of
        ParseOk p ->
            cont p
        ParseError ->
            ParseError

    errorRecover p t ts ms cont = case ms of
        VirtualBrace _:ms -> parse p '}' \r -> case r of
            ParseOk p ->
                runParserL p t ts ms cont
            ParseError ->
                ParseError
        _ ->
            ParseError

    tryClose p t ts ms = case ms of
        []   -> ParseError
        m:ms -> case m of
            VirtualBrace _ -> runSimpleParserL p '}' \p ->
                tryClose p t ts ms
            ExplicitBrace -> case t of
                "}" -> runSimpleParserL p '}' \p ->
                    withL p ts ms
                _ ->
                    ParseError
            ExplicitDBrace _ -> case t of
                t == "}}" -> runSimpleParserL p '}' \p ->
                    withL p ts ms
                _ ->
                    ParseError
            NoLayout
                | t match interp_string_continue -> runSimpleParserL p t \p ->
                    withL p ts (NoLayout:ms)
                | otherwise -> runSimpleParserL p t ts \p ts ->
                    withL p ts ms

    tryEnd p ms = case ms of
        [] ->
            ParseOk p
        m:ms -> case m of
            VirtualBrace _ -> runSimpleParserL p '}' \p ->
                tryEnd p ms
            _ ->
                ParseError

    resolveBraceOpen p ts ms = case ts of
        ts0@(Token _ t:ts) -> case t of
            "{" -> runParserL p '{' ts ms \p ts ms ->
                withL p ts (ExplicitBrace:ms)
            "{{" -> runParserL p '{' ts ms \p ts ms ->
                let m = calcLayoutPos ts
                in withL p ts (ExplicitDBrace m:ms)
            _ -> runParserL p '{' ts0 ms \p ts ms ->
                let m = calcLayoutPos ts
                in withL p ts (VirtualBrace m:ms)
        _ -> runParserL p '{' ts ms \p ts ms ->
            let m = calcLayoutPos ts
            in withL p ts (VirtualBrace m:ms)

    resolveNewline n p ts ms = case ms of
        ExplicitDBrace m:_ ->
            | n < m ->
                ParseError
            | n == m -> runSimpleParserL p ';' \p ->
                withL p ts ms
            | otherwise ->
                withL p ts ms
        VirtualBrace m:ms1
            | n < m -> runSimpleParserL p '}' \p ->
                resolveNewline n p ts ms1
            | n == m -> runSimpleParserL p ';' \p ->
                withL p ts ms
            | otherwise ->
                withL p ts ms
        _ ->
            withL p ts ms

    calcLayoutPos ts = case ts of
        []              -> 0
        Token m _:_     -> m
        Newline m:_     -> m
        ExpectBrace:ts  -> calcLayoutPos ts

    isOpen t = case t of
        "("     -> True
        "["     -> True
        "{"     -> True
        "{{"    -> True
        _ | t match interp_string_start
                -> True
        _       -> False

    isClose t = case t of
        ")"     -> True
        "]"     -> True
        "}"     -> True
        "}}"    -> True
        _ | t match interp_string_end
                -> True
        _       -> False

Fixity Resolution
-----------------

Reference
---------

* `Unicode Identifier and Pattern Syntax <https://unicode.org/reports/tr31/>`_
* `Unicode Character Database - 5.7.1 General Category Values <http://www.unicode.org/reports/tr44/#General_Category_Values>`_
