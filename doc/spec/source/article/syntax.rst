Syntax
======

Lexical Syntax
--------------

.. math::

  \begin{array}{llll}
    \mathit{reservedid}
    &: &\texttt{alias} \\
    &\mid &\texttt{as} \\
    &\mid &\texttt{case} \\
    &\mid &\texttt{data} \\
    &\mid &\texttt{default} \\
    &\mid &\texttt{derive} \\
    &\mid &\texttt{do} \\
    &\mid &\texttt{family} \\
    &\mid &\texttt{fixity} \\
    &\mid &\texttt{foreign} \\
    &\mid &\texttt{implicit} \\
    &\mid &\texttt{import} \\
    &\mid &\texttt{in} \\
    &\mid &\texttt{infix} \\
    &\mid &\texttt{infixl} \\
    &\mid &\texttt{infixr} \\
    &\mid &\texttt{let} \\
    &\mid &\texttt{module} \\
    &\mid &\texttt{newtype} \\
    &\mid &\texttt{of} \\
    &\mid &\texttt{pattern} \\
    &\mid &\texttt{rec} \\
    &\mid &\texttt{self} \\
    &\mid &\texttt{type} \\
    &\mid &\texttt{where} \\
    &\mid &\texttt{with} \\
    &\mid &\texttt{\_} \\

    \mathit{reservedop}
    &: &\texttt{=} \\
    &\mid &\texttt{:} \\
    &\mid &\texttt{->} \\
    &\mid &\texttt{=>} \\
    &\mid &\texttt{@} \\
    &\mid &\texttt{\textasciitilde} \\
    &\mid &\texttt{|} \\
    &\mid &\texttt{?} \\
    &\mid &\texttt{!} \\
    &\mid &\texttt{<-}
  \end{array}

Grammar
-------

.. math::

  \begin{array}{llll}
    \mathit{program}
    &: &\mathit{moddecl} \\
    &\mid &\mathit{body} \\

    \mathit{moddecl}
    &: &\texttt{module}\;\mathit{modid}\;\texttt{where}\;\mathit{body} &(\text{TODO: consider export visibility}) \\

    \mathit{body}
    &: &\texttt{\{}\;\mathit{impdecls}\;\mathit{topdecls}\;\texttt{\}} \\

    \mathit{impdecls}
    &: &\mathit{impdecl}\;\texttt{;}\;\mathit{impdecls} \\
    &\mid \\

    \mathit{impdecl}
    &: &\texttt{import}\;\mathit{modid} &(\text{TODO: consider some functionality: aliases, expand, etc.}) \\

    \mathit{topdecls}
    &: &\mathit{topdecl}\;\texttt{;}\;\mathit{topdecls} \\
    &\mid \\

    \mathit{topdecl}
    &: &\mathit{moddecl} \\
    &\mid &\texttt{type}\;\mathit{decltype}\;\texttt{=}\;\mathit{type} \\
    &\mid &\texttt{type}\;\texttt{family}\;\text{TODO} \\
    &\mid &\texttt{data}\;\mathit{decltype}\;\texttt{where}\;\texttt{\{}\;\mathit{condecls}\;\texttt{\}} \\
    &\mid &\texttt{data}\;\texttt{family}\;\text{TODO} \\
    &\mid &\texttt{data}\;\texttt{newtype}\;\text{decltype}\;\texttt{=}\;\mathit{type} \\
    &\mid &\texttt{foreign}\;\text{TODO} \\
    &\mid &\mathit{decl} \\

    \mathit{decls}
    &: &\mathit{decl}\;\texttt{;}\;\mathit{decls} \\
    &\mid \\

    \mathit{decl}
    &: &\mathit{var}\;\texttt{:}\;\mathit{expsig} \\
    &\mid &\mathit{funlhs}\;\mathit{funrhs} \\
    &\mid &\texttt{fixity}\;\text{TODO} \\
    &\mid &\texttt{implicit}\;\text{TODO} \\
    &\mid &\texttt{pattern}\;\text{TODO} \\

    \mathit{funlhs}
    &: &\mathit{var}\;\mathit{apats} \\
    &\mid &\mathit{apat}\;\mathit{varop}\;\mathit{apats} \\

    \mathit{funrhs}
    &: &\texttt{=}\;\mathit{exp}\;\texttt{where}\;\texttt{\{}\mathit{decls}\texttt{\}} &(\text{TODO: consider some functionality: guards, prebindings, etc.}) \\

    \mathit{exps}
    &: &\mathit{exp}\;\texttt{,}\;\mathit{exps} \\
    &\mid &\mathit{exp} \\
    &\mid \\

    \mathit{exp}
    &: &\mathit{infixexp}\;\texttt{:}\;\mathit{expsig} \\
    &\mid &\mathit{infixexp} \\

    \mathit{infixexp}
    &: &\mathit{blockexp}\;\mathit{qop}\;\mathit{infixexp} \\
    &\mid &\mathit{blockexp} \\

    \mathit{blockexp}
    &: &\texttt{\textbackslash}\;\mathit{apats}\;\texttt{->}\;\mathit{exp} &(\text{TODO: consider some functionality: same as funrhs}) \\
    &\mid &\texttt{let}\;\texttt{\{}\;\mathit{decls}\;\texttt{\}}\;\texttt{in}\;\mathit{exp} \\
    &\mid &\texttt{let}\;\texttt{rec}\;\texttt{\{}\;\mathit{decls}\;\texttt{\}}\;\texttt{in}\;\mathit{exp} &(\text{TODO: maybe use letrec instead}) \\
    &\mid &\texttt{case}\;\mathit{exps}\;\texttt{of}\;\texttt{\{}\;\mathit{alts}\;\texttt{\}} \\
    &\mid &\texttt{\textbackslash}\;\texttt{case}\;\texttt{->}\;\texttt{\{}\;\mathit{alts}\;\texttt{\}} \\
    &\mid &\texttt{do}\;\texttt{\{}\;\mathit{stmts}\;\texttt{\}} \\
    &\mid &\mathit{fexp} \\

    \mathit{fexp}
    &: &\mathit{fexp}\;\mathit{aexp} \\
    &\mid &\mathit{aexp} \\

    \mathit{aexp}
    &: &\texttt{(}\;\mathit{exp}\;\texttt{)} \\
    &\mid &\mathit{qvar} \\
    &\mid &\mathit{gcon} \\
    &\mid &\mathit{literal} \\
    &\mid &\mathit{littuple} \\
    &\mid &\mathit{litvector} \\
    &&&(\text{TODO: consider some atomic expressions: section, etc.}) \\

    \mathit{var}
    &: &\mathit{varid} \\
    &\mid &\texttt{(} \mathit{varsym} \texttt{)} \\

    \mathit{varop}
    &: &\mathit{varsym} \\
    &\mid &{}^\backprime \mathit{varid}\,{}^\backprime \\

    \mathit{qvar}
    &: &\mathit{qvarid} \\
    &\mid &\texttt{(} \mathit{qvarsym} \texttt{)} \\

    \mathit{qvarop}
    &: &\mathit{qvarsym} \\
    &\mid &{}^\backprime \mathit{qvarid}\,{}^\backprime
  \end{array}

Note:

* ``if`` 式はいれない．以下の標準関数で代用::

    if :> a. Bool -> { then: a, else: a } -> a
    if = \case
      True  e -> e.then
      False e -> e.else

Layout
------

TODO: ``{`` / ``}`` でレイアウトオフ，``{{`` / ``}}`` で明示的に終端示すレイアウト．

Fixity Resolution
-----------------
