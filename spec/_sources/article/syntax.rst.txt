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
    zero or more repetitions

  ``( pattern )``
    grouping

  ``pattern | pattern``
    choice

  ``pattern<pattern>``
    difference

  ``"..."``
    terminal by unicode properties

Lexical Syntax
--------------

.. productionlist::
    lexical_program: (lexeme | whitespace)*
    lexeme  : literal
            : special
            : semis
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
    reserved_id : "alias"
                : "as"
                : "match"
                : "data"
                : "derive"
                : "do"
                : "export"
                : "family"
                : "foreign"
                : "impl"
                : "infix"
                : "in"
                : "letrec"
                : "let"
                : "module"
                : "newtype"
                : "none"
                : "pattern"
                : "record"
                : "rec"
                : "role"
                : "signature"
                : "static"
                : "trait"
                : "type"
                : "use"
                : "when"
                : "with"
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
    interp_open: "$" ( "{#" | "⦃" )
    interp_close: "#}" | "⦄"
    interp_string_without_interp: interp_str_open (bstr_graphic<"$"> | uni_escape)* str_sep
    interp_string_start: interp_str_open (bstr_graphic<"$"> | uni_escape)* interp_open
    interp_string_cont: interp_close (bstr_graphic<"$"> | uni_escape)* interp_open
    interp_string_end: interp_close (bstr_graphic<"$"> | uni_escape)* str_sep

.. productionlist::
    whitespace: whitestuff+
    whitestuff  : whitechar
                : comment

.. productionlist::
    comment : line_comment
            : doc_comment
            : pragma_comment
            : multiline_comment
    line_comment: "--" "-"* (any<symbol | other> any*)? newline
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
    "->": "->" | "→"
    "..": ".." | "…"
    "<-": "<-" | "←"
    "<=": "<=" | "⇐"
    "=>": "=>" | "⇒"
    "\\/": "\\/" | "∀"
    "\\": "\\" | "λ"
    "{{": "{{" | "❴"
    "}}": "}}" | "❵"

Layout
------

.. code-block::

    PosToken(t) = ...

.. code-block::

    IsWhitespace(t)         = t match whitespace
    IncludeNewline(t)       = t match (ANY* newline ANY*)
    IsBraceKeyword(t)       = t match ("do" | "record" | "signature"
                                        | "when" | "where" | "with"
                                        )
    IsDBraceKeyword(t)      = t match "let"
    IsDBraceCloseKeyword(t) = t match "in"

.. code-block::

    PostProcess ts                          = <{{>:PostProcess1 ts

    PostProcess1 []                         = []
    PostProcess1 (t:ts)
        | IsWhitespace(t) & IncludeNewline(t) = <;>:PostProcess1 ts
        | IsWhitespace(t)                     = PostProcess1 ts
        | IsBraceKeyword(t)                   = t:<{>:PostProcess1 ts
        | IsDBraceKeyword(t)                  = t:<{{>:PostProcess1 ts
        | IsDBraceCloseKeyword(t)             = <}}>:t:PostProcess1 ts
        | otherwise                           = t:PostProcess1 ts

.. code-block::

    Layout ts = Layout1 ts []

    Layout1 (t:ts) ms
        | t == "("              = t:Layout1 ts (<"(">:ms)
        | t == "["              = t:Layout1 ts (<"[">:ms)
        | t == "{"              = t:Layout1 ts (<"{">:ms) -- For simple record.
        | t match interp_string_start
                                = t:Layout1 ts (<"${#":ms>)
    Layout1 (t:ts) (<"${#">:ms)
        | t match interp_string_continue
                                = t:Layout1 ts (<"${#":ms>)
        | t match interp_string_end
                                = t:Layout1 ts ms
    Layout1 (t:ts) (<b>:ms)
        | t == ")" & b == "("   = t:Layout1 ts ms
        | t == "]" & b == "["   = t:Layout1 ts ms
        | t == "}" & b == "{"   = t:Layout1 ts ms
        | t == "`" & b == "`"   = t:Layout1 ts ms
    Layout1 (t:ts) ms
        | t == "`"              = t:Layout1 ts (<"`">:ms)
    Layout1 (<{>:t:ts) ms
        | t == "{"              = t:Layout1 ts (<"{",0>:ms)
        | t == "{{"             = t:Layout2 "c" ts ms
        | otherwise             = "{":Layout1 (t:ts) (<"{",PosToken(t)>:ms)
    Layout1 (<{{>:t:ts) ms
        | t == "{"              = t:Layout1 ts (<"{",0>:ms)
        | t == "{{"             = t:Layout2 "vc" ts ms
        | otherwise             = "{{":Layout2 "v" (t:ts) ms
    Layout1 [t] ms
        | t == <{>              = "{":Layout1 [] (<"{",1>:ms)
        | t == <{{>             = "{{":Layout1 [] (<"{{","v",1>:ms)
    Layout1 (<;>:t:ts) ms@(<"{",m>:rms)
        | PosToken(t) == m      = ";":Layout1 (t:ts) ms
        | PosToken(t) <  m      = "}":Layout1 (<;>:t:ts) rms
        | otherwise             = Layout1 (t:ts) ms
    Layout1 (<;>:t:ts) ms@(<"{{",_,m>:_)
        | PosToken(t) == m      = ";":Layout1 (t:ts) ms
        | PosToken(t) <  m      = ParseError -- Broken layout by a shallower token.
        | otherwise             = Layout1 (t:ts) ms
    Layout1 (<;>:ts) ms         = Layout1 ts ms
    Layout1 ("}":ts)        (<"{",0>:rms)
                                = "}":Layout1 ts rms
    Layout1 ("}}":<}}>:ts)  (<"{{","vc",_>:rms)
                                = "}}":Layout1 ts rms
    Layout1 ("}}":ts)       (<"{{","c",_>:rms)
                                = "}}":Layout1 ts rms
    Layout1 (<}}>:ts)       (<"{{","v",_>:rms)
                                = "}}":Layout1 ts rms
    Layout1 (t:ts) _
        | t match interp_string_continue
                                = ParseError -- Not corresponding braces.
        | t match interp_string_end
                                = ParseError -- Not corresponding braces.
        | t == "`"              = ParseError -- Not corresponding quotes.
        | t == ")"              = ParseError -- Not corresponding brackets.
        | t == "]"              = ParseError -- Not corresponding brackets.
        | t == "}"              = ParseError -- Not corresponding braces.
        | t == "}}"             = ParseError -- Not corresponding braces.
        | t == <}}>             = ParseError -- Not corresponding braces.
    Layout1 (t:ts) ms           = t:Layout1 ts ms
    Layout1 [] (<"{",m>:rms)
        | m == 0                = ParseError -- Braces are not enough.
        | otherwise             = "}":Layout1 [] rms
    Layout1 [] (<"{{",k,_>:rms)
        | k == "c"              = ParseError -- Braces are not enough.
        | k == "vc"             = ParseError -- Braces are not enough.
        | k == "v"              = "}}":Layout1 [] rms
    Layout1 [] []               = []

    Layout2 k []      ms  = Layout1 [] (<"{{",k,0>:ms)
    Layout2 k (t:ts)  ms  = Layout1 (t:ts) (<"{{",k,PosToken(t)>:ms)

Grammar
-------

.. productionlist::
    program: module_decl_body

.. productionlist::
    module_decl: "module" simplecon "where" module_decl_body
    module_decl_body: "{{" module_decl_items "}}"
                    : "{" module_decl_items "}"
    module_decl_items: (module_decl_item semis)* module_decl_item?
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
    type_decl: "type" simpletype "=" type ("where" type_decl_where)?
    type_decl_where : "{{" type_decl_where_items "}}"
                    : "{" type_decl_where_items "}"
    type_decl_where_items: (type_decl_where_item semis)* type_decl_where_item?
    type_decl_where_item: type_decl
                        : use_clause

.. productionlist::
    type_family_decl: "type" "family" con (":" type)? ("where" ctypefam_decl_body)?
                    : "data" "family" con (":" type)? ("where" cdatafam_decl_body)?
    ctypefam_decl_body  : "{{" ctypefam_decl_items "}}"
                        : "{" ctypefam_decl_items "}"
    ctypefam_decl_items: (ctypefam_decl_item semis)* ctypefam_decl_item?
    ctypefam_decl_item: typefam_impl_decl
    cdatafam_decl_body  : "{{" cdatafam_decl_items "}}"
                        : "{" cdatafam_decl_items "}"
    cdatafam_decl_items: (cdatafam_decl_item semis)* cdatafam_decl_item?
    cdatafam_decl_item: datafam_impl_decl

.. productionlist::
    type_impl_decl  : typefam_impl_decl
                    : datafam_impl_decl
    typefam_impl_decl: "type" "impl" type_impl_decl_type "=" type ("where" type_decl_where)?
    datafam_impl_decl   : "data" "impl" type_impl_decl_type "where" data_decl_body
                        : "newtype" "impl" type_impl_decl_type "=" type ("where" type_decl_where)?
    type_impl_decl_type : con type_qualified*
                        : type_qualified conop type_qualified

.. productionlist::
    data_decl: "data" con (":" type)? "where" data_decl_body
            : "newtype" simpletype "=" type ("where" type_decl_where)?
    data_decl_body: "{{" data_decl_items "}}"
                    : "{" data_decl_items "}"
    data_decl_items: (data_decl_item semis)* data_decl_item?
    data_decl_item: consig_decl

.. productionlist::
    val_decl: simpleval "=" expr ("where" val_decl_where)?
    val_bind: pat "=" expr ("where" val_decl_where)?
    val_decl_where  : "{{" val_decl_where_items "}}"
                    : "{" val_decl_where_items "}"
    val_decl_where_items: (val_decl_where_item semis)* val_decl_where_item?
    val_decl_where_item: let_bind_item

.. productionlist::
    pattern_decl: "pattern" "_" (":" type)? "of" pattern_decl_body
                : "pattern" simplecon "=" pat
                : "pattern" simplecon "<-" pat
    pattern_decl_body   : "{{" pattern_decl_items "}}"
                        : "{" pattern_decl_items "}"
    pattern_decl_items: (pattern_decl_item semis)* pattern_decl_item?
    pattern_decl_item   : simplecon "=" pat
                        : simplecon "<-" pat

.. productionlist::
    trait_decl: "trait" simpletype ("<=" context)* "where" trait_decl_body
    trait_decl_body : "{{" trait_decl_items "}}"
                    : "{" trait_decl_items "}"
    trait_decl_items: (trait_decl_item semis)* trait_decl_item?
    trait_decl_item : sig_item
                    : fixity_decl

.. productionlist::
    impl_decl: "impl" impl_decl_type ("<=" context)* ("for" con)? "where" impl_decl_body
    impl_decl_type  : con type_qualified*
                    : type_qualified conop type_qualified
    impl_decl_body  : "{{" impl_decl_items "}}"
                    : "{" impl_decl_items "}"
    impl_decl_items: (impl_decl_item semis)* impl_decl_item?
    impl_decl_item: module_decl_item

.. productionlist::
    fixity_decl: "infix" infix_assoc infix_prec (op ",")* op ","?
    infix_assoc: "none" | "<-" | "->"
    infix_prec: integer

.. productionlist::
    use_clause: "use" (string ":")?  (con ".")* use_body
    use_items   : use_item
                : "(" (use_item ",")* use_item? ")"
                : "(" ".." ")"
    use_item: con ("as" con)?
            : conop ("as" conop)?
            : var ("as" var)?
            : op ("as" op)?

.. productionlist::
    simpletype  : con bind_var*
                : bind_var conop bind_var
    simplecon   : con bind_var*
                : bind_var conop bind_var
    simpleval   : var bind_var*
                : bind_var op bind_var

.. productionlist::
    type: "\\/" bind_var* "." type
        : context "=>" type
        : type_expr
    context: type_unit
    type_expr   : type_unit "->" type
                : type_unit
    type_unit: type_infix
    type_infix: type_apps (qual_conop type_apps)*
    type_apps: type_qualified type_app*
    type_app: type_qualified
            : "@" type_qualified
    type_qualified: (con ".")* type_atomic ("." type_atomic)*
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
    type_record_body: "{{" type_record_items "}}"
                    : "{" type_record_items "}"
    type_record_items: (type_record_item semis)* type_record_item?
    type_record_item: valsig_decl
    sig_body: "{{" sig_items "}}"
            : "{" sig_items "}"
    sig_items: (sig_item semis)* sig_item?
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
    expr_qualified: (con ".")* expr_block ("." expr_block)*
    expr_block  : "\\" "with" match_body
                : "\\" "when" guarded_alt_body
                : "\\" lambda_body
                : "let" let_binds "in" expr
                : "match" (expr ",")* expr? "with" case_body
                : "do" do_body
                : expr_atomic
    expr_atomic: "(" expr ")"
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
    expr_record_body: "{{" expr_record_items "}}"
                    : "{" expr_record_items "}"
    expr_record_items: (expr_record_item semis)* expr_record_item?
    expr_record_item: valsig_decl
                    : val_decl

.. productionlist::
    pat : pat_unit ("|" pat_unit)*
        : pat_unit ":" type
        : pat_unit
    pat_unit: pat_infix
    pat_infix: pat_apps (qual_conop  pat_apps)*
    pat_apps: type_qualified type_app*
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
    let_binds   : "{{" let_bind_items "}}"
                : "{" let_bind_items "}"
    let_bind_items: (let_bind_item semis)* let_bind_item?
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
    match_body  : "{{" match_alt_items "}}"
                : "{" match_alt_items "}"
    match_alt_items: (match_alt_item semis)* match_alt_item?
    match_alt_item: (pat ",")* pat? guarded_alt
    guarded_alt : "->" expr
                : "when" guarded_alt_body
    guarded_alt_body: "{{" guarded_alt_items "}}"
                    : "{" guarded_alt_items "}"
    guarded_alt_items: (guarded_alt_item semis)* guarded_alt_item?
    guarded_alt_item: guard_qual "->" expr
    guard_qual: expr

.. productionlist::
    do_body : "{{" do_stmt_items "}}"
            : "{" do_stmt_items "}"
    do_stmt_items   : (do_stmt_item semis)* expr semis?
    do_stmt_item    : expr
                    : pat "<-" expr
                    : "let" let_binds
                    : val_bind

.. productionlist::
    bind_var: simple_bind_var
            : "(" simple_bind_var ":" type ")"
            : "@" simple_bind_var
            : "@" "(" simple_bind_var ":" type ")"
    simple_bind_var : var_id
                    : "_"
    con: con_id
        : "(" ")"
        : "(" ( "->" | con_sym ) ")"
    conop: "->" | con_sym
        : "`" con_id "`"
    var: var_id
        : "_"
        : "(" var_sym ")"
    op: var_sym
        : "`" var_id "`"
    qual_conop: (con ".")* conop
    qual_op: (con ".")* op
    semis: ";"*

Note:

* ``if`` 式はいれない．以下の標準関数で代用::

    if : \/a. Bool -> { then: a, else: a } -> a
    if = \case
      True,  e -> e.then
      False, e -> e.else

* multi way if は lambda case で代替::

    func1 : \/a. Int -> a -> Maybe a
    func1 = \case
      0, x -> Just x
      i, x when
        i > 10 -> Just x
        else   -> Nothing

    func2 : Int -> a -> Maybe a
    func2 = \i x -> \case
        when
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

Fixity Resolution
-----------------

Reference
---------

* `Unicode Identifier and Pattern Syntax <https://unicode.org/reports/tr31/>`_
* `Unicode Character Database - 5.7.1 General Category Values <http://www.unicode.org/reports/tr44/#General_Category_Values>`_
