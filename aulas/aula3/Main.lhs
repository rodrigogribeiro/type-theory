\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{Introdução ao $\lambda$-Cálculo Atipado}
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

%if False
 
> module Main where

> import Control.Monad (liftM)
> import Data.Functor
 
> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as Token
> import Text.ParserCombinators.Parsec.Language(haskellDef)
 
%endif
 
\begin{document}
   \begin{frame}
       \titlepage
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (I)}
      \begin{block}{O $\lambda$-cálculo}
         \begin{itemize}
            \item Sistema formal capaz de representar qualquer função computável.
            \item Pode ser vista como linguagem de programação mínima.
            \item Inicialmente concebida para uso em lógica.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (II)}
      \begin{block}{Sintaxe}
         \[
             \begin{array}{rcll}
                t & ::= & x & \text{variável} \\
                  &     & \lambda x. t & \text{abstração}\\
                  &     & t\:t & \text{aplicação}
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (III)}
      \begin{block}{Escopo}
         \begin{itemize}
            \item Variável ligada: $x$ é ligada se ocorre no termo $t$ em $\lambda x.t$.
            \item Variável livre: $x$ é livre se não ocorre no escopo de uma abstração envolvendo $x$.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (IV)}
      \begin{block}{Semântica Operacional}
         \begin{itemize}
            \item Elemento fundamental: substituição.
            \[
                 (\lambda x.t_1)\: t_2 \to [x \mapsto t_2]\:t_1
            \]
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (V)}
      \begin{block}{Representando Booleanos}
         \[
             \begin{array}{lcl}
                true & = & \lambda t. \lambda f. t\\
                false & = & \lambda t. \lambda f. f\\
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (VI)}
      \begin{block}{Funções sobre Booleanos}
         \[
             \begin{array}{lcl}
                not & = & \lambda x . x\: false\: true
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (VII)}
      \begin{block}{Exemplo}
         \[
             \begin{array}{lc}
                not\:\: true & = \\
                (\lambda x .\, x\: false\: true)\: true & = \\
                true\:\: false\:\: true & = \\
                (\lambda t. \lambda f. t)\: false\: true & = \\
                \lambda f. false & = \\
                false & = \\
                \lambda t. \lambda f. f
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (VIII)}
      \begin{block}{Representando números}
         \[
             \begin{array}{lcl}
                0 & = & \lambda s. \lambda z. z \\
                1 & = & \lambda s. \lambda z. s\: z \\
                  & ... & \\
                suc & = & \lambda n. \lambda s. \lambda z. s (n\:\:s\:\:z)
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (IX)}
      \begin{block}{Operações sobre números}
         \[
             \begin{array}{lcl}
                plus & = & \lambda m.\lambda n.\lambda n.\lambda z. m\:\:s\:\:(n\:\:s\:\:z)\\
                times & = & \lambda m.\lambda n. m\:\:(plus\:\:n)\:\:0\\
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (X)}
      \begin{block}{Formalidades}
         \begin{itemize}
            \item Cálculo de variáveis livres
            \[
                \begin{array}{lcl}
                   fv(x) & = & \{x\}\\
                   fv(\lambda x.t_1) & = & fv(t_1) - \{x\}\\
                   fv(t_1\:\:t_2) & = & fv(t_1) \cup fv(t_2)\\
                \end{array}
            \]
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XI)}
      \begin{block}{Substituição}
           \[
               \begin{array}{lcll}
                  {[x \mapsto s]}\:x               & = & s                              & \\
                  {[x \mapsto s]}\:y               & = & y                              & x \neq y\\
                  {[x \mapsto s]}\:(\lambda y.t_1) & = & \lambda y. {[x \mapsto s]}\: t_1 & y \neq x \land y \not\in fv(s)\\
                  {[x \mapsto s]}\:(t_1\:\:t_2)    & = & ({[x\mapsto s]}\:t_1)({[x\mapsto s]}\:t_2)\\
               \end{array}
           \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XII)}
      \begin{block}{Alguns conceitos...}
         \begin{itemize}
            \item Captura de variável.
            \item Termos fechados, termos $\alpha$-equivalentes.
            \item Valores: $\lambda$-abstrações.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XIII)}
      \begin{block}{Semântica Operacional}
          \[
              \begin{array}{c}
                  \fbox{$t\to t'$}\\ \\
                  \begin{array}{c}
                     \infer[_{(EAppAbs)}]
                           {\lambda x. t_{12}\:v_2 \to [x\mapsto v_2]\:t_{12}}
                           {} \\ \\
                     \infer[_{(EApp_1)}]
                           {t_1\:\:t_2\to t'_1\:\:t_2}
                           {t_1\to t'_1} \\ \\
                     \infer[_{(EApp_2)}]
                           {v_1\:\:t_2\to v_1\:\:t'_2}
                           {t_2 \to t'_2}
                  \end{array}
              \end{array}
          \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XIV)}
      \begin{block}{Implementando a sintaxe}

> data LTerm = LVar String
>            | LLam String LTerm
>            | LApp LTerm LTerm
>            deriving (Eq, Ord, Show)

      \end{block}
   \end{frame}

   \begin{frame}{$\lambda$-Cálculo Atipado --- (XV)}
      \begin{block}{Termos $\alpha$-equivalentes}
         \begin{itemize}
            \item Termos equivalentes: $\lambda x. x$ e $\lambda y.y$.

> t1 = LLam "x" (LVar "x")
> t2 = LLam "y" (LVar "y")

            \item Mas o teste de igualdade gerado pelo GHC não corresponde a isso,
                  pois o resultado de |t1 == t2| é \eval{t1 == t2}.
            \item Ideal: Termos $\alpha$-equivalentes reconhecidos pelo
                  teste de igualdade de Haskell.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XVI)}
      \begin{block}{Substituição}
         \begin{itemize}
             \item Outro problema, é a implementação de substituição.
             \item Como implementá-la evitando o problema de captura de variáveis?
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XVII)}
      \begin{block}{Substituição}
         \begin{itemize}
            \item Existem várias técnicas na literatura para o problema de captura de variáveis em substituições.
            \begin{itemize}
                \item Índices DeBruijn
                \item High Order Abstract Syntax
                \item Locally Nameless Representation
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XVIII)}
      \begin{block}{Índices DeBruijn}
          \begin{itemize}
             \item Utilizaremos esta técnica neste curso.
             \item Porquê? Se implementada incorretamente,
                   esta falha de maneira catastrófica, ao
                   invés de situações específicas.
             \item Termos $\alpha$-equivalentes são sintaticamente iguais.
          \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XIX)}
      \begin{block}{Índices DeBruijn}
         \begin{itemize}
            \item Idéia: Representar variáveis como a distância da variável ao $\lambda$ correspondente.
            \item Exemplos:
            \[
                \begin{array}{ll}
                   \hline
                   \text{Termos} & \text{DeBruijn}\\ \hline
                   \lambda x . x & \lambda . 0    \\
                   \lambda x . \lambda y. y x & \lambda . \lambda . 0\:1\\
                \end{array}
            \]
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XX)}
      \begin{block}{Definição formal de termos DeBruijn}
        Denominamos por $\mathcal{T}_n$, $n\in\mathbb{N}$, a família de conjuntos $\{\mathcal{T}_0,\mathcal{T}_1...\}$, em que cada $\mathcal{T}_i$,
        $0 \leq i < n$:
        \[
            \begin{array}{ll}
               k \in \mathcal{T}_n, & 0 \leq k < n ; \\
               \text{se }t\in \mathcal{T}_{n} \text{ então}&\lambda.t\in\mathcal{T}_{n - 1} ; \\
               \text{se }t,t'\in \mathcal{T}_{n} \text{ então}&t\:t'\in\mathcal{T}_{n - 1} ; \\
            \end{array}
        \]
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XXI)}
      \begin{block}{Significado dos conjuntos $\mathcal{T}_n$}
         \begin{itemize}
            \item Elementos de $\mathcal{T}_n$ são termos com no máximo $n\in\mathbb{N}$ variáveis livres.
            \item Conjunto $\mathcal{T}_0$: conjunto de termos fechados.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XXII)}
      \begin{block}{Índices DeBruijn}
         \begin{itemize}
            \item Resolvem facilmente o problema de testar $\alpha$-equivalência.
            Igualdade sintática (implementada por Haskell) identifica tais termos.
           \item Mas, como lidar com variáveis livres? Como converter termos para DeBruijn e vice-versa?
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XXII)}
      \begin{block}{Contextos de nomes}
         \begin{itemize}
            \item Função finita de nomes em índices. Utilizado para converter
                  termos com nomes para notação DeBruijn e vice-versa.
            \item Exemplo: $\Delta =\{x\mapsto 2, y\mapsto 1, z \mapsto 0\}$. Então, o termo
                  $2\:(1\:0)$ representa $x\,(y\:z)$.
            \item Podemos usar uma lista de nomes e a posição destes como seu índice. Com isso,
                  o contexto reduz-se a uma lista ao invés de uma função finita.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{$\lambda$-Cálculo Atipado --- (XXIII)}
      \begin{block}{Convertendo entre notações}
 
      \end{block}
   \end{frame}
\end{document}
