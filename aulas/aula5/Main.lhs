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

> import Control.Monad.Identiry
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Writer
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
                        {\Gamma \vdash t : \Bool & \Gamma \vdash : t' \tau & \Gamma \vdash t'' : t}\\ \\
             \end{array}
          \]
          \end{itemize}
      \end{block}
   \end{frame}
\end{document}
