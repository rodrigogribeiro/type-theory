\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{Extensões ao $\lambda$-Cálculo Tipado Simples}
\subtitle{Teoria de Tipos}
\author[Prof. Rodrigo Ribeiro]{Prof. Rodrigo Ribeiro}
\institute{Departamento de Computação e Sistemas}
\date{\today}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

\DeclareMathAlphabet{\mathkw}{OT1}{cmss}{bx}{n}
%subst keyword a = "\K{" a "}"
%subst conid a = "\C{" a "}"
%subst varid a = "\V{" a "}"
%subst numeral a = "\N{" a "}"

%options ghci

%format :+: = "\C{\:\oplus\:}"
%format :-> = "\V{" -> "}"
%format :*: = "\V{\:\times\:}"

\newcommand{\redFG}[1]{\textcolor[rgb]{0.6,0,0}{#1}}
\newcommand{\greenFG}[1]{\textcolor[rgb]{0,0.4,0}{#1}}
\newcommand{\blueFG}[1]{\textcolor[rgb]{0,0,0.8}{#1}}
\newcommand{\orangeFG}[1]{\textcolor[rgb]{0.8,0.4,0}{#1}}
\newcommand{\purpleFG}[1]{\textcolor[rgb]{0.4,0,0.4}{#1}}
\newcommand{\yellowFG}[1]{\textcolor{yellow}{#1}}
\newcommand{\brownFG}[1]{\textcolor[rgb]{0.5,0.2,0.2}{#1}}
\newcommand{\blackFG}[1]{\textcolor[rgb]{0,0,0}{#1}}
\newcommand{\whiteFG}[1]{\textcolor[rgb]{1,1,1}{#1}}
\newcommand{\yellowBG}[1]{\colorbox[rgb]{1,1,0.2}{#1}}
\newcommand{\brownBG}[1]{\colorbox[rgb]{1.0,0.7,0.4}{#1}}

\newcommand{\ColourStuff}{
  \newcommand{\red}{\redFG}
  \newcommand{\green}{\greenFG}
  \newcommand{\blue}{\blueFG}
  \newcommand{\orange}{\orangeFG}
  \newcommand{\purple}{\purpleFG}
  \newcommand{\yellow}{\yellowFG}
  \newcommand{\brown}{\brownFG}
  \newcommand{\black}{\blackFG}
  \newcommand{\white}{\whiteFG}
}

\ColourStuff

\newcommand{\D}[1]{\blue{\mathsf{#1}}}
\newcommand{\C}[1]{\green{\mathsf{#1}}}
\newcommand{\F}[1]{\green{\mathsf{#1}}}
\newcommand{\V}[1]{\blue{\mathit{#1}}}
\newcommand{\N}[1]{\purple{\mathit{#1}}}
\newcommand{\K}[1]{\red{\mathkw{#1}}}

\newcommand{\suc}[1]{\texttt{suc }#1}
\newcommand{\pred}[1]{\texttt{pred }#1}
\newcommand{\iszero}[1]{\texttt{iszero }#1}
\newcommand{\true}[0]{\texttt{true}}
\newcommand{\false}[0]{\texttt{false}}
\newcommand{\iif}[3]{\texttt{if }#1\texttt{ then }#2\texttt{ else }#3}
\newcommand{\Nat}[0]{\texttt{Nat}}
\newcommand{\Bool}[0]{\texttt{Bool}}
\newcommand{\erase}[1]{\texttt{erase(\ensuremath{#1})}}
\newcommand{\unit}[0]{\texttt{unit}}
\newcommand{\Unit}[0]{\texttt{Unit}}
\newcommand{\deff}[0]{\ensuremath{\stackrel{def}{=}}}
\newcommand{\fv}[1]{\ensuremath{fv(#1)}}
\newcommand{\ann}[2]{\ensuremath{#1\:\texttt{as}\:#2}}
\newcommand{\llet}[2]{\ensuremath{\texttt{let}\:#1\:\texttt{in}\:#2}}
\newcommand{\inl}[1]{\ensuremath{\texttt{inl}\:#1}}
\newcommand{\inr}[1]{\ensuremath{\texttt{inr}\:#1}}
\newcommand{\ccase}[5]{\ensuremath{\texttt{case}\:#1\:\texttt{of}\:\{\inl\:#2\Rightarrow #3;\:\inr\:#4\Rightarrow\:#5\}}}
\newcommand{\fix}[1]{\ensuremath{\texttt{fix}(#1)}}
\newcommand{\lletrec}[2]{\ensuremath{\texttt{letrec}\:#1\:\texttt{in}\:#2}}

%if False

> {-#LANGUAGE TupleSections#-}

> module Main where

> import Control.Monad
> import Control.Monad.Identity
> import Control.Monad.Reader
> import Control.Monad.Writer
> import Control.Monad.Error
> import Control.Monad.State
> import qualified Data.Map as Map
> import Data.Map (Map)
> import Data.Maybe (isNothing, fromJust)
> import Text.ParserCombinators.Parsec

Terms and types structures
==========================

> newtype Name = Name {out :: String}
>                deriving (Eq, Ord, Show)

> data Ty = TBool | TUnit | Ty :-> Ty | Ty :*: Ty
>           | Ty :+: Ty
>           deriving (Eq, Ord, Show)

> data Term =
>   TTrue | TFalse |
>   Var Name |
>   Lam Name Ty Term |
>   App Term Term |
>   Unit |
>   If Term Term Term |
>   Pair Term Term |
>   Fst Term | Snd Term |
>   Inl Term | Inr Term |
>   Case Term Name Term Name Term |
>   Fix Term
>   deriving (Eq, Ord, Show)


> data XTerm  =
>   XTrue | XFalse |
>   XVar Name | XLam Name Ty XTerm |
>   XApp XTerm XTerm | XUnit |
>   XIf XTerm XTerm XTerm |
>   Seq XTerm XTerm | -- sequence
>   Wild Ty XTerm   | -- wildcards
>   Ann XTerm Ty    |
>   Let Name XTerm XTerm |
>   XPair XTerm XTerm |
>   XFst XTerm |
>   XSnd XTerm |
>   XInl XTerm Ty |
>   XInr XTerm Ty |
>   XCase XTerm Ty Name XTerm Name XTerm |
>   XLetRec Name Ty XTerm XTerm
>   deriving (Eq, Ord, Show)

Type checking and elaboration structures
========================================

> type Env = Map Name Ty
 
> type TcElabM a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

> runElabM :: TcElabM a -> (Either String a, Integer)
> runElabM comp = runIdentity (runStateT (runErrorT (runReaderT comp Map.empty)) 0)

> freshName :: TcElabM Name
> freshName
>     = do
>         v <- get
>         put (v + 1)
>         return (Name ("x" ++ (show v)))

> extendEnv :: Name -> Ty -> Env -> Env
> extendEnv n t = Map.insert n t

> lookupEnv :: Name -> TcElabM Ty
> lookupEnv n
>      = do
>        t <- liftM (Map.lookup n) ask
>        maybe (varNotFound n) return t

> varNotFound :: Name -> TcElabM a
> varNotFound = throwError . (++) "Variable Not Found" . out

> typeMismatch :: Ty -> Ty -> TcElabM a
> typeMismatch f e = throwError ("Expected:" ++ show e ++ "\nFound:" ++ show f)

> arrowExpected :: Ty -> TcElabM a
> arrowExpected = throwError . ("Expected arrow type, but found:\n" ++) . show
 
> pairExpected :: Ty -> TcElabM a
> pairExpected = throwError . ("Expected pair type, but found:\n" ++) . show
 
> sumExpected :: Ty -> TcElabM a
> sumExpected = throwError . ("Expected sum type, but found:\n" ++) . show


Type checking and elaboration algorithm
========================================

> check :: XTerm -> TcElabM (Term, Ty)
> check XTrue = return (TTrue , TBool)
> check XFalse = return (TFalse, TBool)
> check XUnit = return (Unit, TUnit)
> check v@(XVar n)
>   = do
>       t <- lookupEnv n
>       return (Var n,t)
> check e@(XLam n ty t)
>   = do
>       (t', ty') <- local (extendEnv n ty) (check t)
>       return (Lam n ty t', ty :-> ty')
> check e@(XApp l r)
>   = do
>       (l',t) <- check l
>       (r',t') <- check r
>       (a,b) <- maybe (arrowExpected t)
>                return
>                (unfoldArr t)
>       when (a /= t') (typeMismatch t' a)
>       return (App l' r', t')
> check x@(XIf c e e')
>      = do
>           (c', tc) <- check c
>           when (tc /= TBool) (typeMismatch TBool tc)
>           (e1, t1) <- check e
>           (e2, t2) <- check e'
>           when (t1 /= t2) (typeMismatch t1 t2)
>           return (If c' e1 e2,t1)
> check (Seq t t')
>      = do
>         (e,ty) <- check t
>         when (ty /= TUnit) (typeMismatch ty TUnit)
>         (e',ty') <- check t'
>         n <- freshName
>         return ((App (Lam n TUnit e) e') , ty')
> check (Wild ty t)
>      = do
>         (e,ty') <- check t
>         n <- freshName
>         return ((Lam n ty e), ty :-> ty')
> check (Ann t ty)
>      = do
>         (e,ty') <- check t
>         when (ty /= ty')(typeMismatch ty' ty)
>         return (e , ty')
> check (Let n t t')
>      = do
>         (e,ty) <- check t
>         (e',ty') <- local (extendEnv n ty)(check t')
>         return (App (Lam n ty e') e, ty')
> check (XPair t t')
>      = do
>         (e, ty) <- check t
>         (e',ty') <- check t'
>         return (Pair e e', ty :*: ty')
> check (XFst t)
>      = do
>         (e,ty) <- check t
>         maybe (pairExpected ty)
>               (return . (Fst e, ) . fst)
>               (unfoldPair ty)
> check (XSnd t)
>      = do
>         (e,ty) <- check t
>         maybe (pairExpected ty)
>               (return . (Snd e, ) . snd)
>               (unfoldPair ty)
> check (XInl t ty)
>      = do
>         (e, ty') <- check t
>         return (Inl e, ty' :+: ty)
> check (XInr t ty)
>      = do
>         (e, ty') <- check t
>         return (Inr e, ty :+: ty')
> check (XCase t ty n' t' n'' t'')
>      = do
>         (e, ty1) <- check t
>         let v = unfoldSum ty1
>         when (isNothing v)
>              (sumExpected ty1)
>         let (l,r) = fromJust v
>         (e',tyl) <- local (extendEnv n' l) (check t')
>         (e'',tyr) <- local (extendEnv n'' r) (check t'')
>         when (tyl /= tyr) (typeMismatch tyl tyr)
>         return (Case e n' e' n'' e'', tyl)
> check (XLetRec n ty t t')
>      = do
>          (e,ty') <- local (extendEnv n ty) (check t)
>          (e',ty'') <- local (extendEnv n (ty :-> ty')) (check t')
>          return (App (Lam n ty e') (Fix (Lam n ty e)), ty'')


> unfoldSum :: Ty -> Maybe (Ty , Ty)
> unfoldSum (t :+: t') = Just (t , t')
> unfoldSum _ = Nothing

> unfoldArr :: Ty -> Maybe (Ty,Ty)
> unfoldArr (l :-> r) = Just (l,r)
> unfoldArr _ = Nothing

> unfoldPair :: Ty -> Maybe (Ty,Ty)
> unfoldPair (l :*: r) = Just (l,r)
> unfoldPair _ = Nothing

%endif

\begin{document}
   \begin{frame}
      \titlepage
   \end{frame}
   \begin{frame}{Extensões --- (I)}
      \begin{block}{$\lambda$-Cálculo Tipado Simples}
         \begin{itemize}
            \item Linguagem de programação estaticamente tipada \textbf{decepcionante}.
            \item Porquê?
            \begin{itemize}
               \item Não possui tipos dados como: string's, num. ponto fluante.
               \item Não possui listas ou registros.
               \item Não possui recursão.
               \item Não permite ``reuso'' de código por meio de definições locais.
            \end{itemize}
            \item Nossa missão: estender o STLC de maneira a torná-lo minimamente útil como linguagem de programação.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (II)}
      \begin{block}{Elaboração de termos}
         \begin{itemize}
            \item Ao projetar linguagens, é útil considerarmos o mínimo de construções.
            \begin{itemize}
               \item Facilita a formalização, uma vez que detalhes não relevantes são desconsiderados.
            \end{itemize}
            \item Açúcar sintático: Construções de uma linguagem que podem ser expressas utilizando outras.
            \item Elaboração (desugaring): Processo de transformação de açúcar sintático em
                  construções básicas da linguagem.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (III)}
      \begin{block}{Setup inicial}
         \begin{spec}
 data Ty = TBool | Ty :-> Ty
           deriving (Eq, Ord, Show)


 data Term =
   TTrue | TFalse |
   Var Name |
   Lam Name Ty Term |
   App Term Term |
   If Term Term Term
   deriving (Eq, Ord, Show)

         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (IV)}
      \begin{block}{Introduzindo novos tipos básicos}
         \begin{itemize}
            \item Tipo básico: tipo que não pode ser definido usando mecanismos da própria linguagem.
            \item Ex: String, float, etc...
            \item Como introduzir um novo tipo básico:
            \begin{itemize}
                \item Estender a linguagem de tipos (adicionando o tipo em questão).
                \item Estender a sintaxe de programas (adicionando constantes deste tipo).
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (V)}
      \begin{block}{Tipo \Unit}
         \begin{itemize}
            \item Tipo que possui um único valor: \unit.
            \item Útil para representar resultados de funções que
                  geram efeitos colaterais.
            \item Usado para representar sequenciamento de funções como açúcar sintático.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (VI)}
      \begin{block}{Extensões para o tipo \Unit}
          \[
              \begin{array}{lclr}
                 t & ::=  & ...   & \text{termos}\\
                   & \mid & \unit & \text{constante }\unit\\
                 v & ::=  & ...   & \text{valores} \\
                   & \mid & \unit & \text{valor }\unit\\
              \tau & ::=  & ...   & \text{tipos básicos}\\
                   & \mid & \Unit & \text{tipo }\Unit
              \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (VII)}
      \begin{block}{Extensões para o tipo \Unit}
         \[
             \infer[_{(TUnit)}]{\Gamma \vdash \unit : \Unit}{}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (VIII)}
      \begin{block}{Açúcar Sintático: Sequenciamento}
          \[
              \begin{array}{lcl}
                 t_1\, ; \, t_2 & \deff & (\lambda x:\Unit . t_2)\: t_1 \\
                                 &      & \text{onde }x\not\in\fv{t_2}
              \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (IX)}
      \begin{block}{Açúcar Sintático: Sequenciamento}
         \[
             \begin{array}{cc}
                 \infer[_{(ESeq)}]{t_1\:;\:t_2\to t'_1\:;\: t_2}
                                  {t_1 \to t'_1}
                 &
                 \infer[_{(ESeqNext)}]{\unit\:;\:t_2\to t_2}{}
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (X)}
      \begin{block}{Açúcar Sintático: Sequenciamento}
         \[
             \infer[_{(TSeq)}]{\Gamma\vdash t_1\:;\:t_2 : \tau}
                   {\Gamma \vdash t_1 : \Unit & \Gamma \vdash t_2 : \tau}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XI)}
      \begin{block}{Açúcar Sintático: Wildcards}
          \[
              \begin{array}{lclr}
                 t & ::=  & ...   & \text{termos}\\
                   & \mid & \lambda \_:\tau . t & \text{wildcard }\\
                 v & ::=  & ...   & \text{valores} \\
                   & \mid & \lambda \_:\tau . t & \text{wildcard }\\
              \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XII)}
      \begin{block}{Açúcar Sintático: Wildcards}
         \[
             \begin{array}{c}
                \infer[_{(TWild)}]{\Gamma \vdash \lambda\_:\tau_1 . t : \tau_1 \to \tau_2}
                                  {\Gamma \vdash t : \tau_2} \\ \\
                 (\lambda\_ : \tau_1 . t)\:v_2 \to t
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XIII)}
      \begin{block}{Implementando extensões relacionadas a \Unit}
        \begin{itemize}
           \item Sintaxe núcleo: tipo |Term|.
           \item Sintaxe derivada: tipo |XTerm|.
        \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XIV)}
      \begin{block}{Sintaxe estendida}
        \begin{spec}
 data XTerm  =
   XTrue | XFalse |
   XVar Name | XLam Name Ty XTerm |
   XApp XTerm XTerm | XIf XTerm XTerm XTerm |
   XUnit |
   Seq XTerm XTerm |
   Wild Ty XTerm
   deriving (Eq, Ord, Show)
       \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XV)}
      \begin{block}{Verificação de tipos e elaboração}
         \begin{spec}
 check :: XTerm -> TcElabM (Term, Ty)
 check XUnit = return (Unit, TUnit)
...
 check (Seq t t')
      = do
         (e,ty) <- check t
         when (ty /= TUnit) (typeMismatch ty TUnit)
         (e',ty') <- check t'
         n <- freshName
         return ((App (Lam n TUnit e) e') , ty')
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XVI)}
      \begin{block}{Verificação de tipos e elaboração}
         \begin{spec}

 check (Wild ty t)
      = do
         (e,ty') <- check t
         n <- freshName
         return ((Lam n ty e), ty :-> ty')

         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XVII)}
      \begin{block}{Anotações de tipo}
          \[
             \begin{array}{lclr}
                t & ::=   & ... & \text{termos} \\
                  &  \mid & \ann{t}{\tau} & \text{anotação de tipo.}
             \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XVIII)}
      \begin{block}{Anotações de tipo}
         \[
            \begin{array}{cc}
               \infer[_{(EAnn)}]{\ann{v_1}{\tau} \to v_1}{} &
               \infer[_{(EAnn1)}]{\ann{t}{\tau}\to\ann{t'}{\tau}}
                                 {t \to t'} \\ \\
                \multicolumn{2}{c}{
                   \infer[_{(TAnn)}]{\Gamma \vdash \ann{t}{\tau} : \tau}
                         {\Gamma \vdash t : \tau}
               }
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XIX)}
      \begin{block}{Sintaxe estendida}
         \begin{spec}
 data XTerm  =
...
   Ann XTerm Ty
  deriving (Eq, Ord, Show)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XX)}
      \begin{block}{Verificação de tipos e elaboração}
         \begin{spec}
 check :: XTerm -> TcElabM (Term , Ty)
...
 check (Ann t ty)
      = do
         (e,ty') <- check t
         when (ty /= ty')(typeMismatch ty' ty)
         return (e , ty')
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXI)}
      \begin{block}{Termos Let --- Sintaxe}
          \[
             \begin{array}{lclr}
                t & ::=   & ... & \text{termos} \\
                  &  \mid & \llet{x = t}{t} & \text{termo let}
             \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXII)}
      \begin{block}{Termos Let --- Semântica e Tipagem}
         \[
             \begin{array}{c}
               \infer[_{(ELetV)}]{\llet{x = v}{t_1}\to [x \mapsto v]\:t_1}{} \\ \\
               \infer[_{(ELet)}]{\llet{x = t_1}{t_2}\to \llet{x = t'_1}{t_2}}
                                {t_1 \to t'_1} \\ \\

                  \infer[_{(TLet)}]{\Gamma\vdash \llet{x = t_1}{t_2} : \tau}
                                   {\Gamma \vdash t_1 : \tau' &
                                    \Gamma , x : \tau' \vdash t_2 : \tau}

             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXIII)}
      \begin{block}{Sintaxe estendida}
         \begin{spec}
data XTerm =
...
   Let Name XTerm XTerm
   deriving (Eq, Ord, Show)

         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXIV)}
      \begin{block}{Verificação de tipos e elaboração}
         \begin{spec}
 check :: XTerm -> TcElabM (Term, Ty)
...
 check (Let n t t')
      = do
         (e,ty) <- check t
         (e',ty') <- local (extendEnv n ty)(check t')
         return (App (Lam n e') e, ty')
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXV)}
      \begin{block}{Pares e projeções}
         \begin{itemize}
            \item Recurso presente em toda linguagem funcional.
            \item Pode ser visto como açúcar sintático.
            \begin{itemize}
               \item Tradução para o $\lambda$-cálculo atipado.
            \end{itemize}
            \item Adicionaremos como primitivas da linguagem pares e projeções.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXVI)}
      \begin{block}{Modificações na sintaxe}
          \[
             \begin{array}{lclr}
                t & ::=   & ... & \text{termos} \\
                  &  \mid & (t,t) & \text{termo par}\\
                  &  \mid & t.1   & \text{projeção 1}\\
                  &  \mid & t.2   & \text{projeção 2}\\
                v & ::=   & ... & \text{valores}\\
                  & \mid  & (v,v) & \text{par de valores}\\
                \tau & ::= & ... & \text{tipos} \\
                     & \mid & \tau \times \tau & \text{produto}
             \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXVII)}
      \begin{block}{Semântica}
         \[
            \begin{array}{cc}
               \infer[_{(EPairBeta1)}]{(v_1,v_2).1 \to v_1}{} &
               \infer[_{(EPairBeta2)}]{(v_1,v_2).1 \to v_2}{} \\ \\
               \infer[_{(EProj1)}]{t.1 \to t'.1}{t \to t'} &
               \infer[_{(EProj2)}]{t.2 \to t'.2}{t \to t'} \\ \\
               \infer[_{(EPair1)}]{(t_1,t_2) \to (t'_1, t_2)}
                                  {t_1 \to t'_1} &
               \infer[_{(EPair2)}]{(v_1,t_2) \to (v_1, t'_2)}
                                  {t_2 \to t'_2} \\

            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXVIII)}
      \begin{block}{Sistema de tipos}
         \[
            \begin{array}{c}
                \infer[_{(TPair)}]{\Gamma\vdash (t,t') : \tau \times \tau'}
                                  {\Gamma \vdash t : \tau &
                                   \Gamma \vdash t' : \tau'} \\ \\
                \infer[_{(TProj1)}]{\Gamma\vdash t.1 : \tau}
                                   {\Gamma \vdash t : \tau \times \tau'} \\ \\
                \infer[_{(TProj2)}]{\Gamma\vdash t.2 : \tau'}
                                   {\Gamma \vdash t : \tau \times \tau'}

            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXIX)}
      \begin{block}{Alterações na sintaxe núcleo}
         \begin{spec}
 data Ty = ... | Ty :*: Ty
           deriving(Eq, Ord, Show)

 data Term =
...
   Pair Term Term |
   Fst Term | Snd Term
   deriving (Eq, Ord, Show)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXX)}
      \begin{block}{Alterações na sintaxe estendida}
         \begin{spec}

 data XTerm  =
...
   XPair XTerm XTerm |
   XFst XTerm |
   XSnd XTerm
   deriving (Eq, Ord, Show)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXI)}
      \begin{block}{Verificação de tipos e elaboração}
         \begin{spec}
 check (XPair t t')
      = do
         (e, ty) <- check t
         (e',ty') <- check t'
         return (Pair e e', ty :*: ty')
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXII)}
      \begin{block}{Verificação de tipos e elaboração}
         \begin{spec}
 check (XFst t)
      = do
         (e,ty) <- check t
         maybe (pairExpected ty)
               (return . (Fst e, ) . fst)
               (unfoldPair ty)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXIII)}
      \begin{block}{Verificação de tipos e elaboração}
         \begin{spec}
 check (XSnd t)
      = do
         (e,ty) <- check t
         maybe (pairExpected ty)
               (return . (Snd e, ) . snd)
               (unfoldPair ty)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXIV)}
      \begin{block}{Somas e análise de casos}
         \begin{itemize}
            \item Tipo para representar união disjunta.
            \item Suporte a análise de casos.
            \item Novamente, modificações na sintaxe núcleo e estendida.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXV)}
      \begin{block}{Modificações na sintaxe}
          \[
             \begin{array}{lclr}
                t & ::=   & ... & \text{termos} \\
                  &  \mid & \inl{t} & \text{esquerda}\\
                  &  \mid & \inr{t}   & \text{direita}\\
                  &  \mid & \ccase{t}{x}{t'}{x'}{t''}   & \text{análise de casos}\\
                v & ::=   & ... & \text{valores}\\
                  & \mid  & \inl{v} & \text{valor esquerda}\\
                  & \mid  & \inr{v} & \text{valor direita}\\
                \tau & ::= & ... & \text{tipos} \\
                     & \mid & \tau + \tau & \text{soma}
             \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXVI)}
      \begin{block}{Semântica}
         \[
            \begin{array}{c}
               \infer[_{(ECaseInl)}]{\ccase{\inl{v}}{x}{t}{x'}{t'} \to [x\mapsto v]\:t}{}\\ \\
               \infer[_{(ECaseInr)}]{\ccase{\inr{v}}{x}{t}{x'}{t'} \to [x'\mapsto v]\:t'}{}\\ \\
               \infer[_{(EInl)}]{\inl{t}\to\inl{t'}}{t \to t'} \\ \\
               \infer[_{(EInr)}]{\inr{t}\to\inr{t'}}{t \to t'}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXVII)}
      \begin{block}{Semântica}
         \[
            \begin{array}{c}
          \infer[_{(ECase)}]{\ccase{y}{x}{t}{x'}{t'} \to \ccase{y'}{x}{t}{x'}{t'}}{y \to y'}\\ \\
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXVIII)}
      \begin{block}{Sistemas de tipos}
         \[
             \begin{array}{c}
                \infer[_{(TInl)}]{\Gamma\vdash\inl{t} : \tau + \tau'}
                                 {\Gamma\vdash t : \tau} \\ \\
                \infer[_{(TInr)}]{\Gamma\vdash\inr{t} : \tau + \tau'}
                                 {\Gamma\vdash t : \tau'} \\ \\
                \infer[_{(TCase)}]{\Gamma\vdash\ccase{e}{x}{t}{x'}{t'} : \tau}
                 {\Gamma \vdash e : \tau_1 + \tau_2 &
                  \Gamma, x : \tau_1 \vdash t : \tau &
                  \Gamma, x' : \tau_2 \vdash t' : \tau}
             \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XXXIX)}
      \begin{block}{Alterações na sintaxe núcleo}
         \begin{spec}
 data Ty = ...
           | Ty :+: Ty
           deriving (Eq, Ord, Show)

         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XL)}
      \begin{block}{Alterações na sintaxe núcleo}
         \begin{spec}
 data Term =
...
   Inl Term | Inr Term |
   Case Term Name Term Name Term
   deriving (Eq, Ord, Show)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLI)}
      \begin{block}{Alterações na sintaxe estendida}
         \begin{spec}
 data XTerm  =
...
   XInl XTerm Ty |
   XInr XTerm Ty |
   XCase XTerm Ty Name XTerm Name XTerm
   deriving (Eq, Ord, Show)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLII)}
      \begin{block}{Verificação de tipos e elaboração}
        \begin{spec}
 check (XInl t ty)
      = do
         (e, ty') <- check t
         return (Inl e, ty' :+: ty)
 check (XInr t ty)
      = do
         (e, ty') <- check t
         return (Inr e, ty :+: ty')
        \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLIII)}
     \begin{block}{Verificação de tipos e elaboração}
        \begin{spec}
 check (XCase t ty n' t' n'' t'')
      = do
         (e, ty1) <- check t
         let v = unfoldSum ty1
         when (isNothing v)
              (sumExpected ty1)
         let (l,r) = fromJust v
         (e',tyl) <- local (extendEnv n' l) (check t')
         (e'',tyr) <- local (extendEnv n'' r) (check t'')
         when (tyl /= tyr) (typeMismatch tyl tyr)
         return (Case e n' e' n'' e'', tyl)
        \end{spec}
     \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLIV)}
     \begin{block}{Recursão}
        \begin{itemize}
            \item Última extensão que estudaremos.
            \item Permite a escrita de funções recursivas irrestritas.
            \begin{itemize}
               \item O $\lambda$-cálculo possui normalização forte.
            \end{itemize}
        \end{itemize}
     \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLV)}
      \begin{block}{Modificações na sintaxe núcleo}
          \[
             \begin{array}{lclr}
                t & ::=   & ... & \text{termos} \\
                  &  \mid & \fix{t} & \text{ponto fixo}\\
              \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLVI)}
      \begin{block}{Modificações na sintaxe estendida}
          \[
             \begin{array}{lclr}
                t & ::=   & ... & \text{termos} \\
                  &  \mid & \lletrec{x = t}{t'} & \text{função recursiva}\\
              \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLVII)}
      \begin{block}{Semântica}
          \[
            \fix{\lambda x : \tau . t}\to [x \mapsto \fix{\lambda x : \tau . t}]\: t
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLVIII)}
      \begin{block}{Elaboração}
         \[
      \lletrec{x :\tau_1 = t}{t'} \deff \llet{x  = \fix{\lambda x : \tau_1.t}}{t'}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (XLIX)}
      \begin{block}{Sistema de tipos}
         \[
             \infer[_{(TLetrec)}]{\Gamma\vdash \lletrec{x : \tau' \to \tau_1  = t}{t'} : \tau}
                                 {\Gamma , x : \tau'\vdash t : \tau_1 & \Gamma , x : \tau'\to \tau_1 \vdash t' : \tau}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (L)}
      \begin{block}{Alterações na sintaxe núcleo}
        \begin{spec}
 data Term =
...
 |  Fix Term
   deriving (Eq, Ord, Show)
        \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (LI)}
      \begin{block}{Alterações na sintaxe estendida}
        \begin{spec}
 data XTerm  =
...
 | XLetRec Name Ty XTerm XTerm
   deriving (Eq, Ord, Show)
        \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (LII)}
      \begin{block}{Verificação de tipos e elaboração}
         \begin{spec}
 check (XLetRec n ty t t')
      = do
          (e,ty') <- local (extendEnv n ty) (check t)
          (e',ty'') <- local (extendEnv n (ty :-> ty')) (check t')
          return (App (Lam n ty e') (Fix (Lam n ty e)), ty'')
         \end{spec}
      \end{block}
   \end{frame}
\end{document}
 
