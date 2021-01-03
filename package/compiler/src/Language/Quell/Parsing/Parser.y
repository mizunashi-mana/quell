{
module Language.Quell.Parsing.Parser (

) where

import Language.Quell.Prelude

import           Language.Quell.Type.Token (Token (..))
import qualified Language.Quell.Type.Token as Token
import           Language.Quell.Parsing.Spanned (Spanned (..))
import qualified Language.Quell.Parsing.Spanned as Spanned
import           Language.Quell.Parsing.Runner (Runner (..))
import qualified Language.Quell.Parsing.Runner as Runner
}

%token
  'as'       { S TokKwAs }
  'case'     { S TokKwCase }
  'class'    { S TokKwClass }
  'data'     { S TokKwData }
  'default'  { S TokKwDefault }
  'deriving' { S TokKwDeriving }
  'do'       { S TokKwDo }
  'else'     { S TokKwElse }
  'forall'   { S TokKwForall }
  'hiding'   { S TokKwHiding }
  'if'       { S TokKwIf }
  'import'   { S TokKwImport }
  'in'       { S TokKwIn }
  'infix'    { S TokKwInfix }
  'none'     { S TokKwNone }
  'assocL'   { S TokKwAssocLeft }
  'assocR'   { S TokKwAssocRight }
  'instance' { S TokKwInstance }
  'let'      { S TokKwLet }
  'module'   { S TokKwModule }
  'newtype'  { S TokKwNewtype }
  'of'       { S TokKwOf }
  'sig'      { S TokKwSignature }
  'then'     { S TokKwThen }
  'type'     { S TokKwType }
  'where'    { S TokKwWhere }

  '->'       { S TokSymArrow }
  '@'        { S TokSymAt }
  ':'        { S TokSymColon }
  '=>'       { S TokSymDArrow }
  '.'        { S TokSymDot }
  '='        { S TokSymEqual }
  '\\'       { S TokSymLamPref }
  '?'        { S TokSymQuestion }
  '_'        { S TokSymUnderscore }

  '{'        { S TokSpOpenCurly }
  '}'        { S TokSpCloseCurly }
  VOCURLY    { S TokSpOpenVCurly }
  VCCURLY    { S TokSpCloseVCurly }
  '['        { S TokSpOpenBrack }
  ']'        { S TokSpCloseBrack }
  '('        { S TokSpOpenParen }
  ')'        { S TokSpCloseParen }
  '`'        { S TokSpBackquote }
  ','        { S TokSpComma }
  ';'        { S TokSpSemi }

  CONID      { S (TokIdConId _) }
  CONSYM     { S (TokIdConSym _) }
  VARID      { S (TokIdVarId _) }
  VARSYM     { S (TokIdVarSym _) }
  QCONID     { S (TokIdQualConId _) }
  QCONSYM    { S (TokIdQualConSym _) }
  QVARID     { S (TokIdQualVarId _) }
  QVARSYM    { S (TokIdQualVarSym _) }

  CHAR       { S (TokLitChar _) }
  STRING     { S (TokLitString _) }
  INTEGER    { S (TokLitInteger _) }
  RATIONAL   { S (TokLitRational _) }

%monad { Runner }{ >>= }{ return }
%lexer { undefined }{ S TokEndOfSource }
%tokentype { Spanned Token }

%name parseProgram program
%%

program :: { () }
  : module_body   { $1 }


module_decl :: { () }
  : 'module' modid 'where' module_body

module_body :: { () }
  : '{' semis_top '}'       { () }
  | VOCURLY semis_top close { () }


semis_top :: { () }
  : semis top { () }

top :: { () }
  : import_decls_semis top_decls_semis  { () }
  | import_decls_semis top_decls        { () }
  | import_decls1                       { () }


import_decls_semis :: { () }
  : import_decls_semis import_decl semis1 { () }
  | {- empty -}                           { () }

import_decls1 :: { () }
  : import_decls_semis import_decl    { () }

import_decl :: { () }
  : 'import' modid maybe_import_spec  { () }

maybe_import_spec :: { () }
  : import_spec     { () }
  | {- empty -}     { () }

import_spec :: { () }
  : '(' export_list ')'           { () }
  | 'hiding' '(' export_list ')'  { () }

export_list :: { () }
  : export ',' export_list  { () }
  | export                  { () }
  | {- empty -}             { () }


top_decls :: { () }
  : top_decls_semis top_decl  { () }

top_decls_semis :: { () }
  : top_decls_semis top_decl semis1 { () }
  | {- empty -}                     { () }

top_decl :: { () }
  : fixity_decl
  | val_sig_decl
  | val_decl
  | type_sig_decl
  | type_decl
  | class_decl
  | module_decl
  | instance_decl


fixity_decl :: { () }
  : 'fixity' fixity_assoc fixity_prec fixity_ops

fixity_assoc :: { () }
  : 'none'    { () }
  | 'assocL'  { () }
  | 'assocR'  { () }

fixity_prec :: { () }
  : INTEGER { () }

fixity_ops :: { () }
  : fixity_ops ',' fixity_op { () }
  | fixity_op                { () }

fixity_op :: { () }
  : op    { () }
  | qop   { () }


val_sig_decl :: { () }
  : var ':' qtype


class_decl :: { () }
  : 'class' class_header where_class_body

class_header :: { () }
  : context '=>' type { () }
  | type              { () }

where_class_body :: { () }
  : 'where' class_body  { () }
  | {- empty -}         { () }

class_body :: { () }
  : '{' class_decls '}'         { () }
  | VOCURLY class_decls close   { () }

class_decls :: { () }
  : class_decls semis1 class_decl   { () }
  | class_decls semis1              { () }
  | class_decl                      { () }
  | {- empty -}                     { () }

class_decl :: { () }
  : fixity_decl
  | sig_decl
  | type_sig_decl


qtype :: { () }
  : 'forall' type_bind_vars '.' qtype
  | 'forall' type_bind_vars '->' qtype
  | context '=>' qtype
  | type

type_bind_vars :: { () }
  : type_bind_var type_bind_vars  { () }
  | {- empty -}                   { () }

type_bind_var :: { () }
  : type_var                  { () }
  | '(' type_var ':' kind ')' { () }

type_var :: { () }
  : VARID   { () }

context :: { () }
  : btype   { () }

type :: { () }
  : btype             { () }
  | btype '->' qtype  { () }

btype :: { () }
  : type_apps    { () }

type_apps :: { () }
  : type_app            { () }
  | type_apps type_app  { () }

type_app :: { () }
  : atype         { () }
  |


modid :: { () }
  : CONID   { () }
  | QCONID  { () }

var :: { () }
  : VARID           { () }
  | '(' VARSYM ')'  { () }

op :: { () }
  : varop   { () }
  | conop   { () }
  | '->'    { () }

qop :: { () }
  : qvarop  { () }
  | qconop  { () }

varop :: { () }
  : VARSYM          { () }
  | '`' VARID '`'   { () }

conop :: { () }
  : CONSYM          { () }
  | '`' CONID '`'   { () }

qvarop :: { () }
  : QVARSYM         { () }
  | '`' QVARID '`'  { () }

qconop :: { () }
  : QCONSYM         { () }
  | '`' QCONID '`'  { () }


semis :: { () }
  : semis ';'   { $1 }
  | {- empty -} { () }

semis1 :: { () }
  : semis1 ';'  { $1 }
  | ';'         { () }
{
pattern S t = Spanned _ t
}
