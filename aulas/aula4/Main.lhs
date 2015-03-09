\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{Introdução aos Sistemas de Tipos}
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

\newcommand{\suc}[1]{\texttt{suc }#1}
\newcommand{\pred}[1]{\texttt{pred }#1}
\newcommand{\iszero}[1]{\texttt{iszero }#1}
\newcommand{\true}[0]{\texttt{true}}
\newcommand{\false}[0]{\texttt{false}}
\newcommand{\iif}[3]{\texttt{if }#1\texttt{ then }#2\texttt{ else }#3}
\newcommand{\Nat}[0]{\texttt{Nat}}
\newcommand{\Bool}[0]{\texttt{Bool}}

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
   \begin{frame}{Sistemas de Tipos --- (I)}
     \begin{block}{Expressões Aritméticas}
        \begin{itemize}
           \item Considere a seguinte linguagem simples:
        \end{itemize}
        \[
           \begin{array}{lcl}
              e & ::=   & \texttt{true}\\
                &  \mid & \texttt{false}\\
                & \mid  & \texttt{if }e\texttt{ then }e\texttt{ else } e\\
                & \mid  & 0 \\
                & \mid  & \texttt{suc }e\\
                & \mid  & \texttt{pred } e\\
                & \mid  & \texttt{iszero }e
           \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (II)}
     \begin{block}{Valores}
        \[
           \begin{array}{lcl}
              v & ::=  & \texttt{true}\\
                & \mid & \texttt{false}\\
                & \mid & nv\\
                & \\
              nv & ::= & 0 \\
                 & \mid & \texttt{suc }nv
           \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (III)}
     \begin{block}{Semântica Operacional}
        \[
           \begin{array}{c}
              \fbox{$e \to e'$}\\
              \\
              \begin{array}{cc}
                 \infer[_{(ESucc)}]{\suc{e} \to \suc{e'}}
                                   {e \to e'}
                  &
                 \infer[_{(EPredZero)}]{\pred{0}\to 0}{} \\
                 \infer[_{(EPredSuc)}]{\pred{\suc{nv}} \to nv}{}
                  &
                  \infer[_{(EPred)}]{\pred{e}\to\pred{e'}}
                           {e \to e'} \\
                  \\
                  \infer[_{(EIsZeroZero)}]{\iszero{0}\to \true}{}
                  &
                  \infer[_{(EIsZeroSuc)}]{\iszero{\suc{nv}} \to \false}{} \\ \\
              \end{array}
           \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (III)}
     \begin{block}{Semântica Operacional --- Continuação}
        \[
            \begin{array}{c}
               \fbox{$e\to e'$} \\
               \\
               \begin{array}{c}
                  \infer[_{(EIsZero)}]{\iszero{e}\to\iszero{e'}}
                                      {e \to e'} \\
                  \\
                  \infer[_{(EIFTrue)}]{\iif{\true}{e}{e'}\to e}{} \\ \\
                  \infer[_{(EIFFalse)}]{\iif{\false}{e}{e'}\to e'}{} \\ \\
                  \infer[_{(EIF)}]{\iif{e}{ea}{eb}\to\iif{e'}{ea}{eb}}{e\to e'}
               \end{array}
            \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (IV)}
      \begin{block}{Valores, Formas normais e Termos Presos}
         \begin{itemize}
            \item Forma normal: termo sobre o qual nenhuma regra de avaliação se aplica.
            \item Valor: ``resposta'' da execução de um programa.
            \item Termos presos (stuck): Formas normais que não são valores.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (V)}
      \begin{block}{Termos presos}
         \begin{itemize}
             \item Idealmente, não deveriámos sequer executar termos presos.
             \item Porquê? Estes não possuem semântica bem definida.
             \item Como evitar a execução de tais termos?
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (VI)}
      \begin{block}{Como evitar termos presos?}
         \begin{itemize}
            \item Note que termos presos possuem argumentos errados:
            \item Ex. \pred{\true}
            \item Classificar valores e operações utilizando tipos.
            \begin{itemize}
               \item tipo \Nat~ para 0 e \suc
               \item tipo \Bool~ para \true~ e \false
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (VII)}
      \begin{block}{Tipos e semântica}
         \begin{itemize}
            \item Dizer que um termo $e$ possui tipo $T$ significa que $e$ avalia
                  ``de maneira óbvia'' para algum valor de tipo $T$.
            \item ``De maneira óbvia'': estaticamente, sem execução do programa.
            \item Sistemas de tipos são conservativos e incompletos.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Sistema de Tipos --- (VIII)}
      \begin{block}{Relação de Tipagem}
         \[
             \begin{array}{c}
                \fbox{$e : \tau$} \\
                \\
                \begin{array}{cc}
                   \infer[_{(TTrue)}]{\true : \Bool}{} &
                   \infer[_{(TFalse)}]{\false : \Bool}{} \\ \\
                   \multicolumn{2}{c}{
                      \infer[_{(TIF)}]{\iif{e}{e'}{e''} : \tau}
                            {e : \Bool & e' : \tau & e'' : \tau}
                   } \\ \\
                   \infer[_{(TZero)}]{0 : \Nat}{} &
                   \infer[_{(TSuc)}]{\suc{e} : \Nat}{e : \Nat} \\ \\
                   \infer[_{(TPred)}]{\pred{e} : \Nat}{e : \Nat} &
                   \infer[_{(TIsZero)}]{\iszero{e} : \Nat}{e : \Nat}
                \end{array}
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (IX)}
      \begin{block}{Lema de inversão da relação de tipagem}
         \begin{itemize}
            \item se \true : $\tau$, então $\tau = \Bool$.
            \item se \false : $\tau$, então $\tau = \Bool$.
            \item se 0 : $\tau$, então $\tau = \Nat$.
            \item se \iif{$e$}{$e'$}{$e''$} : $\tau$, então $e: \Bool$, $e' : \tau$, $e'' : \tau$.
            \item se \suc{$e$} : $\tau$, então $\tau = \Nat$ e $e : \Nat$.
            \item se \pred{$e$} : $\tau$, então $\tau = \Nat$ e $e : \Nat$.
            \item se \iszero{$e$} : $\tau$, então $\tau = \Bool$ e $e : \Nat$.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (X)}
      \begin{block}{Unicidade de tipos}
         Suponha $e$ arbitrário. Se $e : \tau$ e $e : \tau'$ então $\tau = \tau'$.
      \end{block}
   \end{frame}
   \begin{frame}{Sistemas de Tipos --- (XI)}
      \begin{block}{Segurança}
         \begin{itemize}
            \item Uma linguagem é segura (type safe) se nenhum programa bem tipado é preso (stuck).
            \item Para mostrar essa propriedade, basta provarmos dois teoremas:
            \begin{itemize}
               \item Progresso: Nenhum termo bem tipado é preso, isto é, todo termo bem tipado ou é um valor ou pode ser executado por mais um passo.
               \item Preservação: Se um termo bem tipado executa um passo, o termo resultante também é bem tipado.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
  \begin{frame}{Sistemas de Tipos --- (XII)}
      \begin{block}{Formas canônicas}
         \begin{itemize}
           \item Se $v$ é um valor de tipo \Bool, então $v = \true$ ou $v = \false$.
           \item Se $v$ é um valor de tipo \Nat, então $v = 0$ ou existe $v'$ tal que $v = \suc{v'}$.
         \end{itemize}
      \end{block}
  \end{frame}
  \begin{frame}{Sistemas de Tipos --- (XIII)}
     \begin{block}{Progresso}
        Suponha $e : \tau$. Então, $e$ é um valor ou existe $e'$ tal que $e \to e'$.
     \end{block}
  \end{frame}
  \begin{frame}{Sistemas de Tipos --- (XIV)}
     \begin{block}{Preservação}
         Suponha $e : \tau$. Se $e \to e'$ então $e' : \tau$.
     \end{block}
  \end{frame}
\end{document}
