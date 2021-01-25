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

    INTERP_STRING_WITHOUT_INTERP    { S (Token.LitInterpStringWithoutInterp _) }
    INTERP_STRING_START             { S (Token.LitInterpStringStart _) }
    INTERP_STRING_CONTINUE          { S (Token.LitInterpStringContinue _) }
    INTERP_STRING_END               { S (Token.LitInterpStringEnd _) }

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
    : typefam_impl_decl                 { () }
    | type_decl_where_item              { () }

cdatafam_decl_body :: { () }
    : lopen cdatafam_decl_items lclose  { () }

cdatafam_decl_items :: { () }
    : cdatafam_decl_items_semis cdatafam_decl_item  { () }
    | cdatafam_decl_items_semis                     { () }

cdatafam_decl_items_semis :: { () }
    : cdatafam_decl_items_semis cdatafam_decl_item lsemis   { () }
    | {- empty -}

cdatafam_decl_item :: { () }
    : datafam_impl_decl                 { () }
    | type_decl_where_item              { () }


type_impl_decl :: { () }
    : typefam_impl_decl     { () }
    | datafam_impl_decl     { () }

typefam_impl_decl :: { () }
    : 'type' 'impl' impltype '=' type type_decl_where    { () }

datafam_impl_decl :: { () }
    : 'data' 'impl' impltype data_decl_where                 { () }
    | 'newtype' 'impl' impltype '=' type type_decl_where     { () }


data_decl :: { () }
    : 'data' con may_type_sig data_decl_where           { () }
    | 'newtype' simpletype '=' type type_decl_where     { () }

data_decl_where :: { () }
    : 'where' data_decl_body    { () }
    | {- empty -}               { () }

data_decl_body :: { () }
    : lopen data_decl_items lclose  { () }

data_decl_items :: { () }
    : data_decl_items_semis data_decl_item  { () }
    | data_decl_items_semis                 { () }

data_decl_items_semis :: { () }
    : data_decl_items_semis data_decl_item lsemis   { () }
    | {- empty -}                                   { () }

data_decl_item :: { () }
    : consig_decl


val_decl :: { () }
    : simpleval '=' expr val_decl_where     { () }

val_decl_where :: { () }
    : 'where' val_decl_where_body   { () }
    | {- empty -}

val_decl_where_body :: { () }
    : lopen val_decl_where_items lclose { () }

val_decl_where_items :: { () }
    : val_decl_where_items_semis val_decl_where_item    { () }
    | val_decl_where_items_semis                        { () }

val_decl_where_items_semis :: { () }
    : val_decl_where_items_semis val_decl_where_item lsemis { () }
    | {- empty -}                                           { () }

val_decl_where_item :: { () }
    : let_bind_item     { () }


pattern_decl :: { () }
    : 'pattern' '_' may_type_sig 'of' pattern_decl_body { () }
    | 'pattern' simplecon '=' pat
    | 'pattern' simplecon '<-' pat

pattern_decl_body :: { () }
    : lopen pattern_decl_items lclose   { () }

pattern_decl_items :: { () }
    : pattern_decl_items_semis pattern_decl_item    { () }
    | pattern_decl_items_semis                      { () }

pattern_decl_items_semis :: { () }
    : pattern_decl_items_semis pattern_decl_item lsemis { () }
    | {- empty -}                                       { () }

pattern_decl_item :: { () }
    : simplecon '=' pat         { () }
    | simplecon '<-' pat        { () }


trait_decl :: { () }
    : 'trait' simpletype type_left_contexts trait_decl_where { () }

type_left_contexts :: { () }
    : type_left_contexts '<=' context   { () }
    | {- empty -}                       { () }

trait_decl_where :: { () }
    : 'where' trait_decl_body       { () }

trait_decl_body :: { () }
    : lopen trait_decl_items lclose     { () }

trait_decl_items :: { () }
    : trait_decl_items_semis trait_decl_item        { () }
    | trait_decl_items_semis                        { () }

trait_decl_items_semis :: { () }
    : trait_decl_items_semis trait_decl_item lsemis { () }
    | {- empty -}                                   { () }

trait_decl_item :: { () }
    : sig_item          { () }
    | fixity_decl       { () }


impl_decl :: { () }
    : 'impl' impltype type_left_contexts impl_decl_name impl_decl_where { () }

impl_decl_name :: { () }
    : 'of' con      { () }
    | {- empty -}   { () }

impl_decl_where :: { () }
    : 'where' impl_decl_body    { () }
    | {- empty -}               { () }

impl_decl_body :: { () }
    : lopen impl_decl_items lclose  { () }

impl_decl_items :: { () }
    : impl_decl_items_semis impl_decl_item  { () }
    | impl_decl_items_semis                 { () }

impl_decl_items_semis :: { () }
    : impl_decl_items_semis impl_decl_item lsemis   { () }
    | {- empty -}                                   { () }

impl_decl_item :: { () }
    : module_decl_item      { () }


fixity_decl :: { () }
    : 'infix' infix_assoc infix_prec infix_ops      { () }

infix_assoc :: { () }
    : '->'          { () }
    | '<-'          { () }
    | {- empty -}   { () }

infix_prec :: { () }
    : INTEGER       { () }

infix_ops :: { () }
    : infix_ops_commas op   { () }
    | infix_ops_commas      { () }

infix_ops_commas :: { () }
    : infix_ops_commas op ','   { () }
    | {- empty -}               { () }

use_clause :: { () }
    : 'use' use_package_name qualified_cons use_body

use_body :: { () }
    : '(' '..' ')'          { () }
    | '(' use_items ')'     { () }
    | use_item              { () }

use_items :: { () }
    : use_items_commas use_item     { () }
    | use_items_commas              { () }

use_items_commas :: { () }
    : use_items_commas use_item ',' { () }
    | {- empty -}                   { () }

use_item :: { () }
    : con 'as' con                  { () }
    | conop 'as' conop              { () }
    | var 'as' var                  { () }
    | op 'as' op                    { () }
    | con                           { () }
    | conop                         { () }
    | var                           { () }
    | op                            { () }


simpletype :: { () }
    : con bind_vars                 { () }
    | bind_var conop bind_var       { () }

impltype :: { () }
    : con type_apps_args                        { () }
    | type_qualified conop type_qualified       { () }

simplecon :: { () }
    : con bind_vars                 { () }
    | bind_var conop bind_var       { () }

simpleval :: { () }
    : var bind_vars                 { () }
    | bind_var op bind_var          { () }


type :: { () }
    : '\\/' bind_vars '=>' type     { () }
    | context '=>' type             { () }
    | type_expr                     { () }

context :: { () }
    : type_unit         { () }

type_expr :: { () }
    : type_unit '->' type       { () }
    | type_unit                 { () }

type_unit :: { () }
    : type_infix                { () }

type_infix :: { () }
    : type_infix qual_conop type_apps   { () }
    | type_apps                         { () }

type_apps :: { () }
    : type_qualified type_apps_args     { () }

type_apps_args :: { () }
    : type_apps_args type_app           { () }
    | {- empty -}                       { () }

type_app :: { () }
    : '@' type_qualified        { () }
    | type_qualified            { () }

type_qualified :: { () }
    : qualified_cons type_qualifieds type_atomic { () }

type_qualifieds :: { () }
    : type_qualifieds type_atomic '.'   { () }
    | {- empty -}                       { () }

type_atomic :: { () }
    : '(' type may_type_sig ')'     { () }
    | con                           { () }
    | var                           { () }
    | type_literal                  { () }

type_literal :: { () }
    : literal                           { () }
    | '(' type_tuple_items ')'          { () }
    | '[' type_array_items ']'          { () }
    | '{' type_simplrecord_items '}'    { () }
    | 'record' type_record_body         { () }
    | 'signature' sig_body              { () }

type_tuple_items :: { () }
    : type_tuple_items_commas type ','   { () }
    | type_tuple_items_commas type       { () }

type_tuple_items_commas :: { () }
    : type_tuple_items_commas type ','  { () }
    | type ','                          { () }

type_array_items :: { () }
    : type_array_items_commas type  { () }
    | type_array_items_commas       { () }

type_array_items_commas :: { () }
    : type_array_items_commas type ','  { () }
    | {- empty -}                       { () }

type_simplrecord_items :: { () }
    : type_simplrecord_items_semis type_simplrecord_item    { () }
    | type_simplrecord_items_semis                          { () }

type_simplrecord_items_semis :: { () }
    : type_simplrecord_items_semis type_simplrecord_item ','    { () }
    | {- empty -}                                               { () }

type_simplrecord_item :: { () }
    : var ':' type      { () }

type_record_body :: { () }
    : lopen type_record_items lclose    { () }

type_record_items :: { () }
    : type_record_items_semis type_record_item  { () }
    | type_record_items_semis                   { () }

type_record_items_semis :: { () }
    : type_record_items_semis type_record_item lsemis   { () }
    | {- empty -}                                       { () }

type_record_item :: { () }
    : valsig_decl   { () }

sig_body :: { () }
    : lopen sig_items lclose    { () }

sig_items :: { () }
    : sig_items_semis sig_item  { () }
    | sig_items_semis           { () }

sig_items_semis :: { () }
    : sig_items_semis sig_item lsemis   { () }
    | {- empty -}                       { () }

sig_item :: { () }
    : typesig_decl      { () }
    | valsig_decl       { () }
    | consig_decl       { () }
    | patternsig_decl   { () }
    | use_clause        { () }


expr :: { () }
    : expr_infix ':' type       { () }
    | expr_infix                { () }

expr_infix :: { () }
    : expr_infix qual_conop expr_apps   { () }
    | expr_infix qual_op expr_apps      { () }
    | expr_apps                         { () }

expr_apps :: { () }
    : expr_qualified expr_apps_args     { () }

expr_apps_args :: { () }
    : expr_apps_args expr_app           { () }
    | {- empty -}                       { () }

expr_app :: { () }
    : '@' expr_qualified        { () }
    | expr_qualified            { () }

expr_qualified :: { () }
    : qualified_cons expr_qualifieds expr_block { () }

expr_qualifieds :: { () }
    : expr_qualifieds expr_block '.'    { () }
    | {- empty -}                       { () }

expr_block :: { () }
    : '\\' 'case' case_body
    | '\\' 'when' guarded_alt_body
    | '\\' lambda_body
    | 'let' let_body
    | 'letrec' let_body
    | 'case' case_body
    | 'do' do_body
    | expr_atomic

expr_atomic :: { () }
    : '(' expr ')'                  { () }
    | con                           { () }
    | var                           { () }
    | expr_literal                  { () }

expr_literal :: { () }
    : literal                           { () }
    | expr_interp_string                { () }
    | '(' expr_tuple_items ')'          { () }
    | '[' expr_array_items ']'          { () }
    | '{' expr_simplrecord_items '}'    { () }
    | 'record' expr_record_body         { () }

expr_interp_string :: { () }
    : INTERP_STRING_WITHOUT_INTERP                                          { () }
    | INTERP_STRING_START expr expr_interp_string_conts INTERP_STRING_END   { () }

expr_interp_string_conts :: { () }
    : expr_interp_string_conts INTERP_STRING_CONTINUE expr  { () }
    | {- empty -}                                           { () }

expr_tuple_items :: { () }
    : expr_tuple_items_commas expr ','   { () }
    | expr_tuple_items_commas expr       { () }

expr_tuple_items_commas :: { () }
    : expr_tuple_items_commas expr ','  { () }
    | expr ','                          { () }

expr_array_items :: { () }
    : expr_array_items_commas expr  { () }
    | expr_array_items_commas       { () }

expr_array_items_commas :: { () }
    : expr_array_items_commas expr ','  { () }
    | {- empty -}                       { () }

expr_simplrecord_items :: { () }
    : expr_simplrecord_items_semis expr_simplrecord_item    { () }
    | expr_simplrecord_items_semis                          { () }

expr_simplrecord_items_semis :: { () }
    : expr_simplrecord_items_semis expr_simplrecord_item ','    { () }
    | {- empty -}                                               { () }

expr_simplrecord_item :: { () }
    : var '=' type      { () }

expr_record_body :: { () }
    : lopen expr_record_items lclose    { () }

expr_record_items :: { () }
    : expr_record_items_semis expr_record_item  { () }
    | expr_record_items_semis                   { () }

expr_record_items_semis :: { () }
    : expr_record_items_semis expr_record_item lsemis   { () }
    | {- empty -}                                       { () }

expr_record_item :: { () }
    : valsig_decl       { () }
    | val_decl          { () }


qualified_cons :: { () }
    : qualified_cons con '.'    { () }
    | {- empty -}               { () }

may_type_sig :: { () }
    : ':' type      { () }
    | {- empty -}   { () }


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
