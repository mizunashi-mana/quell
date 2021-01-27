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
            : "`"
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
    module_decl: "module" simplecon ("where" module_decl_body)?
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
    type_decl: "type" simpletype "=" type ("where" type_decl_where_body)?
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
                : "newtype" simpletype "=" type ("where" type_decl_where_body)?
    data_decl_body  : lopen data_decl_items lclose
    data_decl_items: (data_decl_item lsemis)* data_decl_item?
    data_decl_item: consig_decl

.. productionlist::
    val_decl: simpleval "=" expr ("where" val_decl_where_body)?
    val_bind: pat "=" expr ("where" val_decl_where_body)?
    val_decl_where_body : lopen val_decl_where_items lclose
    val_decl_where_items: (val_decl_where_item lsemis)* val_decl_where_item?
    val_decl_where_item: let_bind_item

.. productionlist::
    pattern_decl: "pattern" "_" (":" type)? "of" pattern_decl_body
                : "pattern" simplecon "=" pat
                : "pattern" simplecon "<-" pat
    pattern_decl_body   : lopen pattern_decl_items lclose
    pattern_decl_items: (pattern_decl_item lsemis)* pattern_decl_item?
    pattern_decl_item   : simplecon "=" pat
                        : simplecon "<-" pat

.. productionlist::
    trait_decl: "trait" simpletype ("<=" context)* ("where" trait_decl_body)?
    trait_decl_body : lopen trait_decl_items lclose
    trait_decl_items: (trait_decl_item lsemis)* trait_decl_item?
    trait_decl_item : sig_item
                    : fixity_decl

.. productionlist::
    impl_decl: "impl" impltype ("<=" context)* ("of" con)? ("where" impl_decl_body)?
    impl_decl_body  : lopen impl_decl_items lclose
    impl_decl_items: (impl_decl_item lsemis)* impl_decl_item?
    impl_decl_item: module_decl_item

.. productionlist::
    fixity_decl: "infix" infix_assoc? infix_prec (op ",")* op ","?
    infix_assoc: "<-" | "->"
    infix_prec: integer

.. productionlist::
    use_clause: "use" (string ":")?  (con ".")* use_body
    use_body    : "(" ".." ")"
                : "(" (use_item ",")* use_item? ")"
                : use_item
    use_item: con ("as" con)?
            : conop ("as" conop)?
            : var ("as" var)?
            : op ("as" op)?

.. productionlist::
    export_clause: "export" export_body
    export_body : "(" (export_item ",")* export_item? ")"
                : export_item
    export_item : con ("as" con)?
                : conop ("as" conop)?
                : var ("as" var)?
                : op ("as" op)?

.. productionlist::
    derive_clause: "derive" impltype ("<=" context)* ("of" con)?

.. productionlist::
    simpletype  : con bind_var*
                : bind_var conop bind_var
    impltype    : con type_qualified*
                : type_qualified conop type_qualified
    simplecon   : con bind_var*
                : bind_var conop bind_var
    simpleval   : var bind_var*
                : bind_var op bind_var

.. productionlist::
    type: "\\/" bind_var* "=>" type
        : context "=>" type
        : type_expr
    context: type_unit
    type_expr   : type_unit "->" type
                : type_unit
    type_unit: type_infix
    type_infix: type_apps (qual_conop type_apps)*
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
                : "\\" "when" guarded_alt_body
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
    lambda_body : pat_atomic* "->" expr

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
    do_body : lopen do_stmt_items lclose
    do_stmt_items   : (do_stmt_item lsemis)* expr
    do_stmt_item    : expr
                    : pat "<-" expr
                    : pat "=" expr
                    : ("let" | "letrec") let_binds

.. productionlist::
    bind_var: simple_bind_var
            : "(" simple_bind_var ":" type ")"
            : "@" simple_bind_var
            : "@" "(" simple_bind_var ":" type ")"
    simple_bind_var : var_id
                    : "_"
    con : con_id | "(" ")"
        : "(" ( "->" | con_sym ) ")"
    conop   : "->" | con_sym
            : "`" con_id "`"
    var : var_id | "_"
        : "(" var_sym ")"
    op  : var_sym
        : "`" var_id "`"
    qual_conop: (con ".")* conop
    qual_op: (con ".")* op

.. productionlist::
    lopen: '{' lsemis?
    lclose: lsemis? '}'
    lsemis: ';'+

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

    withL p ts ms = case ts of
        [] -> tryEnd p ms
        t:ts
            | isWhiteSpaceWithNewline t ->
                startNewline p ts ms
            | isWhiteSpace t ->
                withL p ts ms
            | otherwise -> p t \r -> case r of
                ParseOk p
                    | isOpen t  -> withL p ts (<>:ms)
                    | isClose t -> case ms of
                        <>:ms -> withL p ts ms
                        _     -> tryClose p ts ms
                    | t match interp_string_continue -> case ms of
                        <>:ms -> withL p ts (<>:ms)
                        _     -> ParseError
                    | otherwise -> withL p ts ms
                ParseError -> errorRecover p t ts ms

    errorRecover p t ts ms = case ms of
        [] -> errorRecover2 p t ts ms
        m:ms -> p '}' \r -> case r of
            ParseOk p -> case (t, m) of
                ("}", <>)      -> withL p ts ms
                ("}}", <{{,_>) -> withL p ts ms
                (_, <{,_>)     -> withL p (t:ts) ms
            ParseError -> errorRecover2 p t ts (m:ms)

    errorRecover2 p t ts ms = p '{' \r -> case r of
        ParseOk p -> case t of
            "{"  -> withL p ts (<>:ms)
            "{{" -> openDBrace p ts ms
            _    -> openVBrace p (t:ts) ms
        ParseError -> case t of
            ";" -> p ';' \r -> case r of
                ParseOk p -> withL p ts ms
                ParseError -> ParseError
            _   -> ParseError

    tryClose p ts ms = case ms of
        []       -> withL p ts ms
        <{,_>:ms -> p '}' \r -> case r of
            ParseOk p  -> tryClose p ts ms
            ParseError -> ParseError
        <>:ms    -> withL p ts ms
        <{{,_>:_ -> ParseError

    tryEnd p ms = case ms of
        []       -> ParseOk p
        <{,_>:ms -> p '}' \r -> case r of
            ParseOk p  -> tryEnd p ms
            ParseError -> ParseError
        _        -> ParseError

    startNewline p ts ms = case ts of
        [] -> withL p ts ms
        t:ts
            | isWhiteSpace t ->
                startNewline p ts ms
            | otherwise ->
                resolveL (PosToken t) p (t:ts) ms

    resolveL n p ts ms = case ms of
        []       -> withL p ts ms
        <>:_     -> withL p ts ms
        <{,m>:ms
            | n < m -> p '}' \r -> case r of
                ParseOk p  -> resolveL n p ts ms
                ParseError -> ParseError
            | otherwise -> p ';' \r -> case r of
                ParseOk p  -> withL p ts (<{,m>:ms)
                ParseError -> ParseError
        <{{,m>:ms
            | n < m -> ParseError
            | otherwise -> p ';' \r -> case r of
                ParseOk p  -> withL p ts (<{{,m>:ms)
                ParseError -> ParseError

    openDBrace p ts ms = case ts of
        [] -> withL p ts (<{{,0>:ms)
        t:ts
            | isWhiteSpace t ->
                openDBrace p ts ms
            | otherwise ->
                withL p (t:ts) (<{{,PosToken(t)>:ms)

    openVBrace p ts ms = case ts of
        [] -> withL p ts (<{,0>:ms)
        t:ts
            | isWhiteSpace t ->
                openVBrace p ts ms
            | otherwise ->
                withL p (t:ts) (<{,PosToken(t)>:ms)

    isWhiteSpace t =
        t match whitespace

    isWhiteSpaceWithNewline t =
        isWhiteSpace t &&
        t match ANY* newline ANY*

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
