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
  program: (lexeme | whitespace)*
  lexeme: var_id
        : var_op
        : con_id
        : con_op
        : reserved_id
        : reserved_op
        : literal

.. productionlist::
  var_id: (small (small | large | digit | other)*)<reserved_id>
  con_id: (large (small | large | digit | other)*)<reserved_id>
  var_sym: (symbol<":"> (symbol | other)*)<reserved_op>
  con_sym: (":" (symbol | other)*)<reserved_op>

.. productionlist::
  reserved_id: "alias"
             : "as"
             : "case"
             : "data"
             : "default"
             : "derive"
             : "do"
             : "export"
             : "family"
             : "foreign"
             : "impl"
             : "in"
             : "infix"
             : "let"
             : "letrec"
             : "module"
             : "newtype"
             : "none"
             : "of"
             : "pattern"
             : "rec"
             : "record"
             : "role"
             : "signature"
             : "static"
             : "type"
             : "trait"
             : "use"
             : "where"
             : "with"
             : "Self"
             : "_"
  reserved_op: "."
             : ".."
             : ":"
             : "::"
             : "="
             : "=>"
             : "<="
             : "<-"
             : "->"
             : "\\"
             : "|"
             : "@"
             : "~"
             : "?"
             : "!"
             : "#"
  special: "("
         : ")"
         : ","
         : ";"
         : "["
         : "]"
         : "`" -- `
         : "{"
         : "}"

.. productionlist::
  literal: integer
         : rational
         : bytestring
         : string
         : bytechar
         : char

.. productionlist::
  integer: sign? decimal
         : sign? zero ("b" | "B") bit (bit | "_")*
         : sign? zero ("o" | "O") octit (octit | "_")*
         : sign? zero ("x" | "X") hexit (hexit | "_")*
  rational: sign? decimal "." decimal exponent?
          : sign? decimal ("." decimal)? exponent
  decimal: digit (digit | "_")*
  sign: "+"
      : "-"
  zero: "0"
  exponent: ("e" | "E") sign? decimal
  bit: "0" | "1"
  octit: "0" | "1" | ... | "7"
  hexit: digit
       : "A" | "B" | ... | "F"
       : "a" | "b" | ... | "f"

.. productionlist::
  bytestring: "#" str_sep bstr_graphic* str_sep
  string: str_sep (bstr_graphic | uni_escape)* str_sep
  bytechar: "#" char_sep bchar_graphic char_sep
  char: char_sep (bchar_graphic | uni_escape) char_sep
  str_sep: "\""
  char_sep: "'"
  escape_open: "\\"
  bstr_graphic: graphic<str_sep | escape_open>
              : whitechar
              : byte_escape
              : gap
  bchar_graphic: graphic<char_sep | escape_open>
               : " "
               : byte_escape<"\\&">
  byte_escape: escape_open (charesc | asciiesc | byteesc)
  uni_escape: escape_open "u{" hexit+ "}"
  gap: escape_open "|" whitechar* "|"
  charesc: "0" | "a" | "b" | "f" | "n" | "r" | "t" | "v"
         : "&" | escape_open | str_sep | char_sep
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
  whitespace: whitestuff+
  whitestuff: whitechar
            : comment

.. productionlist::
  comment: line_comment
         : multiline_comment
         : doc_comment
  line_comment: "--" "-"* (any<symbol> any*)? newline
  multiline_comment: comment_open ANY<"!"> ANYs (nested_comment ANYs)* comment_close
  doc_comment: comment_open "!" ANY* newline "|" comment_close
  nested_comment: comment_open ANYs (nested_comment ANYs)* comment_close
  comment_open: "{-"
  comment_close: "-}"
  any: graphic | " " | "\t"
  ANYs: (ANY*)<ANY* (comment_open | comment_close) ANY*>
  ANY: graphic | whitechar

.. productionlist::
  graphic: small
         : large
         : symbol
         : digit
         : other
         : special
         : "\""
         : other_graphic
  whitechar: "\p{Pattern_White_Space}"
  newline: "\r\n" | "\r" | "\n" | "\f"
  small: "\p{General_Category=Lowercase_Letter}"
       : "\p{General_Category=Other_Letter}"
       : "_"
  large: "\p{General_Category=Uppercase_Letter}"
       : "\p{General_Category=Titlecase_Letter}"
  symbol: symbolchar<special | "#" | "_" | "\"" | "'">
  symbolchar: "\p{General_Category=Connector_Punctuation}"
            : "\p{General_Category=Dash_Punctuation}"
            : "\p{General_Category=Other_Punctuation}"
            : "\p{General_Category=Symbol}"
  digit: "\p{General_Category=Decimal_Number}"
       : "\p{General_Category=Other_Number}"
  other: "\p{General_Category=Modifier_Letter}"
       : "\p{General_Category=Mark}"
       : "\p{General_Category=Letter_Number}"
       : "'"
  other_graphic: "\p{General_Category=Punctuation}"<symbolchar>

Grammar
-------

.. productionlist::
  program: module_decl_body

.. productionlist::
  module_decl: "module" simplecon "where" module_decl_body
             : "module" simplecon "=" expr
  module_decl_body: "{{" module_decl_items "}}"
                  : "{" module_decl_items "}"
  module_decl_items: (module_decl_item ";"+)* (module_decl_item ";"*)?
  module_decl_item: type_decl
                  : sig_decl
                  : typesig_decl
                  : valsig_decl
                  : module_decl
                  : data_decl
                  : val_decl
                  : trait_decl
                  : impl_decl
                  : fixity_decl
                  : use_clause
                  : foreign_use_clause
                  : derive_clause

.. productionlist::
  type_decl: "type" simpletype "=" type

.. productionlist::
  sig_decl: "signature" simpletype "where" sig_decl_body
  sig_decl_body: "{{" sig_decl_items "}}"
               : "{" sig_decl_items "}"
  sig_decl_items: (sig_decl_item ";"+)* (sig_decl_item ";"*)?
  sig_decl_item: sig_decl
               : typesig_decl
               : valsig_decl
               : use_clause

.. productionlist::
  typesig_decl: "type" con ":" kind
  valsig_decl: var ":" type
  consig_decl: con ":" type

.. productionlist::
  data_decl: "data" con "where" data_decl_body
           : "newtype" simplecon "=" expr
  data_decl_body: "{{" data_decl_items "}}"
                : "{" data_decl_items "}"
  data_decl_items: (data_decl_item ";"+)* (data_decl_item ";"*)?
  data_decl_item: consig_decl
                : use_clause

.. productionlist::
  trait_decl: "trait" simpletype "<=" context "where" trait_decl_body
  trait_decl_body: "{{" trait_decl_items "}}"
                 : "{" trait_decl_items "}"
  trait_decl_items: (trait_decl_item ";"+)* (trait_decl_item ";"*)?
  trait_decl_item: sig_decl
                 : typesig_decl
                 : valsig_decl
                 : fixity_decl
                 : use_clause

.. productionlist::
  simpletype: con { var_id }
            : var_id conop var_id
  simplecon: con { var_id }
           : var_id conop var_id
  simpleval: var { var_id }
           : var_id op var_id

.. productionlist::
  kind:
  type:
  context:
  ctype:
  expr:

.. productionlist::
  kind:
  type:
  con: con_id
       : "(" con_sym ")"
  conop: con_sym
       : "`" con_id "`"
  var: var_id
     : "(" var_sym ")"
  op: var_sym
    : "`" var_id "`"
  qual_con: (con ".")* con
  qual_conop: (con ".")* conop

Note:

* ``if`` 式はいれない．以下の標準関数で代用::

    if : \a -> Bool -> { then: a, else: a } -> a
    if = \case
      True  e -> e.then
      False e -> e.else

* multi way if / lambda case はラムダ抽象で代替::

    func1 : \a -> Int -> a -> Maybe a
    func1 = \
      0 x -> Just x
      i x
        | i > 10 -> Just x
        | else   -> Nothing

    func2 : Int -> a -> Maybe a
    func2 = \i x -> \
      | i == 0 -> Just x
      | i > 10 -> Just x
      | else   -> Nothing


Layout
------

TODO: ``{`` / ``}`` でレイアウトオフ，``{{`` / ``}}`` で明示的に終端示すレイアウト．

Fixity Resolution
-----------------

Reference
---------

* `Unicode Identifier and Pattern Syntax <https://unicode.org/reports/tr31/>`_
* `Unicode Character Database - 5.7.1 General Category Values <http://www.unicode.org/reports/tr44/#General_Category_Values>`_
