\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{Normalização para o $\lambda$-Cálculo Tipado Simples}
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

\newcommand{\norm}[2]{\ensuremath{\texttt{Norm}_{#1}(#2)}}

%if False

> {-# LANGUAGE GADTs, KindSignatures #-}

> module Main where

> data Term :: * -> * where
>   TTrue  :: Term Bool
>   TFalse :: Term Bool
>   Lift   :: a -> Term a
>   Abs    :: (Term a -> Term b) -> Term (a -> b)
>   App    :: Term (a -> b) -> Term a -> Term b

> eval :: Term t -> t
> eval TTrue = True
> eval TFalse = False
> eval (Lift v) = v
> eval (Abs f) = \x -> eval (f (Lift x))
> eval (App l r) = (eval l) (eval r)

%endif

\begin{document}
   \begin{frame}
      \titlepage
   \end{frame}
   \begin{frame}{Normalização --- (I)}
      \begin{block}{Normalização do STLC}
         \begin{itemize}
            \item Todo programa do STLC termina em um número finito de passos.
            \item Fundamentação teórica para:
            \begin{itemize}
               \item Garante a terminação de algoritmos de verificação de
                     tipos para linguagens como Coq e Agda.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (II)}
      \begin{block}{Normalização}
         \textbf{Teorema:} Para todo $e$, $\vdash e : \tau$, existe $v$ tal que $e \Rightarrow^* v$.
         Qual a dificuldade em se provar esse teorema?
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (III)}
      \begin{block}{Qual a falha da ``prova'' anterior?}
         \begin{itemize}
            \item Caso problemático: Aplicação.
            \item Ao fazermos indução sobre o tamanho de um termo ou de derivações do sistema de tipos,
                  não necessariamente uma aplicação terá ``tamanho'' menor --- problema: substituição.
            \item Are we hopeless?
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (IV)}
      \begin{block}{Are we hopeless?}
         \begin{itemize}
            \item Yes... there is hope.
            \item A solução consiste em uma técnica comum em demonstração de teoremas por indução:
                 Obter uma hipótese de indução forte o suficiente.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (V)}
      \begin{block}{Definindo um predicado para normalização}
         \begin{itemize}
            \item O predicado $\norm{\tau}{t}$ é definido como:
            \[
               \begin{array}{lcl}
                 \norm{\tau}{t} & sse & t\text{ pára.}\\
                 \norm{\tau_1 \to \tau_2}{t} & sse & t \text{ pára e, sempre que } \norm{\tau_1}{s}\text{ então }\\
                 & & \norm{\tau_2}{t\: s}.
               \end{array}
            \]
            Definimos $\norm{\tau}{t}$ como sendo o conjunto de todos os termos fechados de tipo
            $\tau$ cuja computação termina em um número finito de passos.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (VI)}
      \begin{block}{Utilidade de $\norm{\tau}{t}$}
         \begin{itemize}
            \item Como essa definição pode nos ajudar na demonstração?
            \item O predicado $\norm{\tau}{t}$ nos ajuda a dividir a prova em duas partes:
            \begin{itemize}
               \item Primeiro mostramos que todo termo $t$ tal que $\norm{\tau}{t}$ pára.
               \item Segundo mostramos que todo termo $t$ tal que $\vdash t : \tau$ é tal que $\norm{\tau}{t}$.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (VII)}
      \begin{block}{Primeira parte da prova}
         \begin{itemize}
             \item Todo termo $t$ tal que $\norm{\tau}{t}$ pára.
             \begin{itemize}
                \item Direto da definição de $\norm{\tau}{t}$.
             \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (VIII)}
      \begin{block}{Segunda parte da prova}
         \begin{itemize}
            \item Divida em dois lemas
            \begin{itemize}
               \item $\norm{\tau}{t}$ é um invariante da semântica.
               \item Todo $\vdash t : \tau$ pertence a $\norm{\tau}{t}$.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (IX)}
      \begin{block}{Primeiro lema}
          Se $\vdash t : \tau$ e $t \Rightarrow t'$, então $\norm{\tau}{t}$ se e somente se $\norm{\tau}{t'}$.
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (X)}
      \begin{block}{Segundo lema}
          Se $x_1 : \tau_1,...,x_n : \tau_n \: \vdash \: t : \tau$ e $v_1,...,v_n$ são valores fechados de tipos $\tau_1,...,\tau_n$ tais que
          $\norm{\tau_i}{v_i}$, para cada $i$ então $\norm{\tau}{[x_1\mapsto v_1]...[x_n\mapsto v_n]\:t}$.
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (XI)}
      \begin{block}{Normalização}
          \textbf{Teorema}: Se $\vdash t : \tau$ então existe $n\in\mathbb{N}$ e $v$ tais que $t \Rightarrow^n v$ e $v$ é um valor.
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (XII)}
      \begin{block}{Implementação}
         \begin{itemize}
            \item Normalização por avaliação.
            \begin{itemize}
               \item Uso do próprio mecanismo de Haskell para executar a avaliação
               \item Uso de duas extensões GADTs e KindSignatures.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (XIII)}
      \begin{block}{Sintaxe tipada}
         \begin{spec}
 data Term :: * -> * where
   TTrue  :: Term Bool
   TFalse :: Term Bool
   Lift   :: a -> Term a
   Abs    :: (Term a -> Term b) -> Term (a -> b)
   App    :: Term (a -> b) -> Term a -> Term b
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Normalização --- (XIV)}
      \begin{block}{Normalização por avaliação}
         \begin{spec}
 eval :: Term t -> t
 eval TTrue = True
 eval TFalse = False
 eval (Lift v) = v
 eval (Abs f) = \x -> eval (f (Lift x))
 eval (App l r) = (eval l) (eval r)
         \end{spec}
      \end{block}
   \end{frame}
\end{document}
