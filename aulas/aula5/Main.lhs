\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{$\lambda$-Cálculo Tipado Simples}
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

%if False

> module Main where

> import Control.Monad.Identity
> import Control.Monad.Reader
> import Control.Monad.Writer
> import Control.Monad.Error
> import qualified Data.Map as Map
> import Data.Map (Map)
> import Text.ParserCombinators.Parsec
 

%endif


\begin{document}
   \begin{frame}
      \titlepage
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (I)}
      \begin{block}{Nas aulas anteriores...}
         \begin{itemize}
            \item Semântica operacional.
            \item Expressões aritméticas tipadas.
            \item Hoje: Como atribuir tipos a funções?
            \begin{itemize}
               \item Como verificar se argumentos possuem tipos apropriados?
               \item Como verificar se resultados são adequados?
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (II)}
      \begin{block}{Sintaxe}
          \[
              \begin{array}{lcll}
                 t & ::= & x & \text{variáveis}\\
                   & \mid & \lambda x : \tau. t & \text{abstração}\\
                   & \mid & t\:\:t & \text{aplicação}\\
                   & \mid & \true & \text{constantes}\\
                   & \mid & \false & \text{constantes}\\
                   & \mid & \iif{t}{t}{t} & \text{condicional}
              \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (III)}
      \begin{block}{Sintaxe}
         \[
             \begin{array}{lcl}
                \tau & ::= & \Bool \\
                     & \mid & \tau \to \tau\\
             \end{array}
         \]
         \begin{itemize}
            \item $\to$: Construtor de tipos funcionais. Associa a direita.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (IV)}
      \begin{block}{Verificando aplicações}
         \begin{itemize}
            \item Como saber se uma aplicação $\lambda x.t$ pode ou não ser aplicada a um termo $t'$?
            \begin{itemize}
               \item Anotar o tipo de $x$ e verificar se $t'$ possui o mesmo tipo.
               \item Inferir o tipo de $x$ com base em seus usos em $t$.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (V)}
      \begin{block}{Verificação de tipos}
         \begin{itemize}
            \item Consideraremos que todo parâmetro de abstração terá seu tipo anotado.
            \item Com isso, basta considerarmos que toda ocorrência da variável possuirá o tipo
                  anotado. Isso nos leva a seguinte regra:
                  \[
                       \infer[]{\vdash\lambda x : \tau . t : \tau \to \tau'}
                               {x : \tau \vdash t : \tau'}
                  \]
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (VI)}
      \begin{block}{Verificação de tipos}
         \begin{itemize}
            \item Como termos podem ter vários $\lambda$ aninhados, precisamos ``armazenar'' variáveis e seus respectivos tipos.
            \item Damos o nome de contexto de tipos a sequência de pares formados por uma variável e seu tipo.
            \item Representamos contextos por $\Gamma$. Regra considerando contextos.
                  \[
                       \infer[]{\Gamma\vdash\lambda x : \tau . t : \tau \to \tau'}
                               {\Gamma,x : \tau \vdash t : \tau'}
                  \]

         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (VII)}
      \begin{block}{Verificação de tipos}
         \begin{itemize}
            \item Regra para variáveis
            \[
                \infer[]{\Gamma\vdash x : \tau}{x : \tau \in \Gamma}
            \]
            \item Regra para aplicação
            \[
                \infer[]{\Gamma\vdash t\:\:t' : \tau}
                        {\Gamma\vdash t : \tau'\to \tau & \Gamma\vdash t' : \tau'}
            \]
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (VIII)}
      \begin{block}{Verificação de tipos}
          \begin{itemize}
             \item Regras para if e constantes
          \[
             \begin{array}{c}
                \infer[]{\Gamma \vdash \true : \Bool}{}\\ \\
                \infer[]{\Gamma \vdash \false : \Bool}{}\\ \\
                \infer[]{\Gamma \vdash \iif{t}{t'}{t''} : \tau}
                        {\Gamma \vdash t : \Bool & \Gamma \vdash t' : \tau & \Gamma \vdash t'' : t}\\ \\
             \end{array}
          \]
          \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (IX)}
      \begin{block}{Propriedades da relação de tipagem}
         \begin{itemize}
            \item Lema de inversão.
            \item Unicidade de tipos.
            \item Lema de formas canônicas.
            \item Progresso.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (X)}
      \begin{block}{Propriedades da relação de tipagem}
         \begin{itemize}
            \item Permutação.
            \item Weakening.
            \item Lema da substituição
            \item Preservação
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XI)}
      \begin{block}{Lema de inversão}
         \begin{itemize}
            \item Se $\Gamma \vdash x : \tau$ então $x : \tau \in \Gamma$.
            \item Se $\Gamma \vdash \true : \tau$ então $\tau = \Bool$.
            \item Se $\Gamma \vdash \false : \tau$ então $\tau = \Bool$.
            \item Se $\Gamma \vdash \lambda x : \tau_1 . t : \tau$, então:
            \begin{itemize}
                \item $\tau = \tau_1 \to \tau_2$.
                \item $\Gamma,x : \tau_1\vdash t : \tau_2$.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XII)}
      \begin{block}{Lema de inversão}
         \begin{itemize}
            \item Se $\Gamma \vdash t\:\:t' : \tau$, então $\Gamma\vdash t : \tau' \to \tau$ e $\Gamma \vdash t' : \tau'$.
            \item Se $\Gamma \vdash \iif{t}{t'}{t''} : \tau$, então:
            \begin{itemize}
               \item $\Gamma \vdash t : \Bool$.
               \item $\Gamma \vdash t' : \tau$.
               \item $\Gamma\vdash t'' : \tau$.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XIII)}
      \begin{block}{Unicidade de tipos}
         Se $\Gamma\vdash t : \tau$ e $\Gamma \vdash t : \tau'$ então $\tau = \tau'$
      \end{block}
      \begin{block}{Lema de formas canônicas}
         \begin{itemize}
            \item Se $v$ é um valor de tipo \Bool então $v = \true$ ou $v = \false$
            \item Se $v$ é um valor de tipo $\tau \to \tau'$ então $v = \lambda x: \tau . t : \tau'$.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XIV)}
      \begin{block}{Progresso}
         Suponha que $\vdash t : \tau$. Então $t$ é um valor ou existe $t'$ tal que $t \to t'$.
      \end{block}
      \begin{block}{Permutação}
         Suponha que $\Gamma \vdash t : \tau$ e seja $\Delta$ uma permutação de $\Gamma$. Então, $\Delta \vdash t : \tau$,
         e ambas as derivações possuem a mesma altura.
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XV)}
      \begin{block}{Weakening}
         Suponha que $\Gamma \vdash t : \tau$ e $x\not\in dom(\Gamma)$. Então, $\Gamma , x : \tau' \vdash t : \tau $ e
         ambas as derivações possuem a mesma altura.
      \end{block}
      \begin{block}{Lema da substituição}
         Suponha que $\Gamma , x : \tau' \vdash t : \tau$ e $\Gamma \vdash t' : \tau'$ então $\Gamma \vdash [x \mapsto t']\:t$.
      \end{block}
      \begin{block}{Preservação}
         Suponha que $\Gamma \vdash t : \tau$ e que $t \to t'$ então $\Gamma \vdash t' : \tau$
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XVI)}
      \begin{block}{O Isomorfismo de Curry-Howard}
         \[
            \begin{array}{cc}
               \lambda-\text{Cálculo} & \text{Lógica}\\ \hline & \\
               \infer[_{(TABS)}]
                     {\Gamma \vdash \lambda x : \tau' \to \tau }
                     {\Gamma , x : \tau' \vdash \tau}
               &
               \infer[_{(\to_I)}]
                     {\Gamma \vdash \tau' \to \tau}
                     {\Gamma , \tau' \vdash \tau} \\ \\
               \infer[_{(TAPP)}]
                     {\Gamma \vdash t\:\:t' : \tau}
                     {\Gamma \vdash t : \tau' \to \tau & \Gamma \vdash : \tau'}
               &
               \infer[_{(\to_E)}]
                     {\Gamma \vdash \tau}
                     {\Gamma \vdash \tau' \to \tau &  \Gamma \vdash \tau'}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XVII)}
      \begin{block}{O Isomorfismo de Curry-Howard}
         \[
             \begin{array}{cc}
                \text{Lógica} & \text{Linguagens de Programação}\\ \hline
                \text{proposições} & \text{tipos}\\
                \text{proposição }P\to Q & \text{tipo } P \to Q \\
                \text{proposição }P \land Q & \text{tipo } P \times Q\\
                \text{Prova de }P & \text{programa de tipo }P\\
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XVIII)}
      \begin{block}{Erasure}
         \[
            \begin{array}{lcl}
               \erase{x} & = & x \\
               \erase{\lambda x:\tau.t} & = & \lambda x.\erase{t}\\
               \erase{t\:\:t'} & = & \erase{t}\:\:\erase{t'}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XIX)}
      \begin{block}{Propriedades da erasure}
         Sejam $t$ e $t'$ tais que $\Gamma \vdash t : \tau$ e $\Gamma\vdash t' : \tau'$.
         \begin{itemize}
            \item Se $t \to t'$, então $\erase{t} \to \erase{t'}$.
            \item Se $\erase{t} \to m'$, então existe $t'$ tal que $t \to t'$ e $\erase{t'} = m'$.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XX)}
      \begin{block}{Implementando um type checker}

> data Term = Var String | Lam String Ty Term
>           | App Term Term | TTrue | TFalse
>           | If Term Term Term
>             deriving (Eq, Ord, Show)

      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XXI)}
      \begin{block}{Implementando um typechecker}

> data Ty = Boolean | Ty :-> Ty deriving (Eq, Ord, Show)

> type Env = Map String Ty


      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XXII)}
      \begin{block}{Implementando um typechecker}

> data TyErr = TyMismatch {
>                 expected :: Ty,
>                 found :: Ty,
>                 expr :: Term
>              } | VariableNotFound String
>                | ExpectingArrow Ty
>                deriving (Eq, Ord)


      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XXIII)}
      \begin{block}{Implementando um typechecker}

> type Check a = ReaderT Env (ErrorT TyErr Identity) a

> lookupEnv :: String -> Env -> Check Ty
> lookupEnv v e = maybe (throwError (VariableNotFound v))
>                       return
>                       (Map.lookup v e)

> extendEnv :: String -> Ty -> Env -> Env
> extendEnv v t = Map.insert v t

      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XXIV)}
      \begin{block}{Implementando um typechecker}

> check :: Term -> Check Ty
> check TTrue = return Boolean
> check TFalse = return Boolean
> check (Var v) = ask >>= lookupEnv v
> check (Lam v ty t)
>       = do
>           ty' <- local (extendEnv v ty)
>                        (check t)
>           return (ty :-> ty')

      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XXV)}
      \begin{block}{Implementando um typechecker}

> check e@(App l r)
>       = do
>          tl <- check l
>          tr <- check r
>          (a,r) <- maybe (throwError (ExpectingArrow tl))
>                         return
>                         (unfoldArr tl)
>          when (tr /= a) (throwError (TyMismatch a tr e))
>          return r

      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Tipado Simples --- (XXVI)}
      \begin{block}{Implementando um typechecker}

> check x@(If c e e')
>      = do
>           tc <- check c
>           when (tc /= Boolean) (throwError (TyMismatch Boolean tc x))
>           t1 <- check e
>           t2 <- check e'
>           when (t1 /= t2) (throwError (TyMismatch t1 t2 x))
>           return t1
 
      \end{block}
   \end{frame}
\end{document}

%if False

> instance Error TyErr where
>    noMsg = VariableNotFound ""
>    strMsg = VariableNotFound

> unfoldArr :: Ty -> Maybe (Ty,Ty)
> unfoldArr (t :-> t') = Just (t,t')
> unfoldArr _ = Nothing

%endif
