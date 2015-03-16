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

%if False

> module Main where

> import Control.Monad
> import Control.Monad.Identity
> import Control.Monad.Reader
> import Control.Monad.Writer
> import Control.Monad.Error
> import Control.Monad.State
> import qualified Data.Map as Map
> import Data.Map (Map)
> import Text.ParserCombinators.Parsec

Terms and types structures
==========================

> newtype Name = Name {out :: String}
>                deriving (Eq, Ord, Show)

> data Ty = TBool | TUnit | Ty :-> Ty
>           deriving (Eq, Ord, Show)

> data Term =
>   TTrue | TFalse |
>   Var Name |
>   Lam Name Ty Term |
>   App Term Term |
>   Unit |
>   If Term Term Term
>   deriving (Eq, Ord, Show)


> data XTerm  =
>   XTrue | XFalse |
>   XVar Name | XLam Name Ty XTerm |
>   XApp XTerm XTerm | XUnit |
>   XIf XTerm XTerm XTerm |
>   Seq XTerm XTerm | -- sequence
>   Wild Ty XTerm    -- wildcards
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

Type checking and elaboration algorithm
========================================

> check :: XTerm -> TcElabM (Term, Ty)
> check XTrue = return (TTrue , TBool)
> check XFalse = return (TFalse, TBool)
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

> unfoldArr :: Ty -> Maybe (Ty,Ty)
> unfoldArr (l :-> r) = Just (l,r)
> unfoldArr _ = Nothing

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
   \begin{frame}{Extensões --- (IV)}
      \begin{block}{Tipo \Unit}
         \begin{itemize}
            \item Tipo que possui um único valor: \unit.
            \item Útil para representar resultados de funções que
                  geram efeitos colaterais.
            \item Usado para representar sequenciamento de funções como açúcar sintático.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (V)}
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
   \begin{frame}{Extensões --- (VI)}
      \begin{block}{Extensões para o tipo \Unit}
         \[
             \infer[_{(TUnit)}]{\Gamma \vdash \unit : \Unit}{}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (VII)}
      \begin{block}{Açúcar Sintático: Sequenciamento}
          \[
              \begin{array}{lcl}
                 t_1\, ; \, t_2 & \deff & (\lambda x:\Unit . t_2)\: t_1 \\
                                 &      & \text{onde }x\not\in\fv{t_2}
              \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{Extensões --- (VIII)}
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
   \begin{frame}{Extensões --- (IX)}
      \begin{block}{Açúcar Sintático: Sequenciamento}
         \[
             \infer[_{(TSeq)}]{\Gamma\vdash t_1\:;\:t_2 : \tau}
                   {\Gamma \vdash t_1 : \Unit & \Gamma \vdash t_2 : \tau}
         \]
      \end{block}
   \end{frame}
\end{document}
