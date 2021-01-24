{
module Language.Quell.Parsing.Parser where

import Language.Quell.Prelude

import qualified Prelude
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Parsing.Spanned as Spanned
}

%token
    'as'            { S Token.KwAs }
    'case'          { S Token.KwCase }
    'data'          { S Token.KwData }
    'default'       { S Token.KwDefault }
    'derive'        { S Token.KwDerive }
    'do'            { S Token.KwDo }
    'in'            { S Token.KwIn }
    'infix'         { S Token.KwInfix }
    'let'           { S Token.KwLet }
    'module'        { S Token.KwModule }
    'newtype'       { S Token.KwNewtype }
    'of'            { S Token.KwOf }
    'signature'     { S Token.KwSignature }
    'type'          { S Token.KwType }
    'where'         { S Token.KwWhere }
    '_'             { S Token.KwUnderscore }

    '->'       { S Token.SymArrow }
    '@'        { S Token.SymAt }
    ':'        { S Token.SymColon }
    '=>'       { S Token.SymDArrow }
    '.'        { S Token.SymDot }
    '='        { S Token.SymEqual }
    '\\'       { S Token.SymLambda }
    '?'        { S Token.SymUnknown }

    '{'        { S Token.SpBraceOpen }
    '}'        { S Token.SpBraceClose }
    VOBRACE    { S Token.SpVBraceOpen }
    VCBRACE    { S Token.SpVBraceClose }
    '['        { S Token.SpBrackOpen }
    ']'        { S Token.SpBrackClose }
    '('        { S Token.SpParenOpen }
    ')'        { S Token.SpParenClose }
    '`'        { S Token.SpBackquote }
    ','        { S Token.SpComma }
    VSEMI      { S Token.SpSemi }

    CONID      { S (Token.IdConId _) }
    CONSYM     { S (Token.IdConOp _) }
    VARID      { S (Token.IdVarId _) }
    VARSYM     { S (Token.IdVarOp _) }

    CHAR       { S (Token.LitChar _) }
    STRING     { S (Token.LitString _) }
    INTEGER    { S (Token.LitInteger _) }
    RATIONAL   { S (Token.LitRational _) }

%monad { ParserWithL }{ >>= }{ return }
%lexer { lexer }{ S Token.EndOfSource }
%tokentype { Spanned.T Token.T }

%name parseProgram program
%%

program :: { () }
    : module_decl_body   { $1 }

module_decl :: { () }
    : 'module' simplecon module_decl_where { () }

module_decl_where :: { () }
    : 'where' module_decl_body  { () }
    : {- empty -}               { () }

module_decl_body :: { () }
    : lopen module_decl_items lclose { () }

module_decl_items :: { () }
    : module_decl_items_semis module_decl_item  { () }
    | module_decl_items_semis                   { () }

module_decl_items_semis :: { () }
    : module_decl_items_semis module_decl_item lsemis   { () }
    : {- empty -}                                       { () }

module_decl_item :: { () }
    : sig_item
    | type_decl
    | type_family_decl
    | type_impl_decl
    | data_decl
    | val_decl
    | module_decl
    | pattern_decl
    | trait_decl
    | impl_decl
    | fixity_decl
    | foreign_val_decl
    | export_clause
    | derive_clause


typesig_decl :: { () }
    : 'type' con ':' type

valsig_decl :: { () }
    : var ':' type

consig_decl :: { () }
    : con ':' type

patternsig_decl :: { () }
    : 'pattern' con ':' type

foreign_val_decl :: { () }
    : 'foreign' STRING var ':' type


type_decl :: { () }
    : 'type' simpletype '=' type type_decl_where    { () }

type_decl_where :: { () }
    : 'where' type_decl_where_body  { () }
    | {- empty -}

type_decl_where_body :: { () }
    : lopen type_decl_where_items lclose    { () }

type_decl_where_items :: { () }
    : type_decl_where_items_semis type_decl_where_item  { () }
    | type_decl_where_items_semis                       { () }

type_decl_where_items_semis :: { () }
    : type_decl_where_items_semis type_decl_where_item lsemis   { () }
    | {- empty -}                                               { () }

type_decl_where_item :: { () }
    : type_decl     { () }
    | use_clause    { () }


type_family_decl :: { () }
    : 'type' 'family' con may_type_sig 'where' ctypefam_decl_body   { () }
    | 'type' 'family' con may_type_sig                              { () }
    | 'data' 'family' con may_type_sig 'where' cdatafam_decl_body   { () }
    | 'data' 'family' con may_type_sig

ctypefam_decl_body :: { () }
    : lopen ctypefam_decl_items lclose  { () }

ctypefam_decl_items :: { () }
    : ctypefam_decl_items_semis ctypefam_decl_item      { () }
    | ctypefam_decl_items_semis                         { () }

ctypefam_decl_items_semis :: { () }
    : ctypefam_decl_items_semis ctypefam_decl_item lsemis   { () }
    | {- empty -}                                           { () }

ctypefam_decl_item :: { () }
    : typefam_impl_decl
    | type_decl_where_item


simplecon :: { () }
    : {- TODO -}    { () }

lopen :: { () }
    : VOBRACE may_lsemis    { () }

lclose :: { () }
    : may_lsemis vclose     { () }

vclose :: { () }
    : VCBRACE   { () }
    | error     { () }

lsemis :: { () }
    : lsemis VSEMI  { () }
    | VSEMI         { () }

may_lsemis :: { () }
    : may_lsemis VSEMI  { () }
    | {- empty -}       { () }
{
pattern S :: Token.T -> Spanned.T Token.T
pattern S t <- Spanned.Spanned
    {
        getSpan = _,
        unSpanned = t
    }

type ParserWithL = Identity

lexer = undefined

happyError = undefined
}
