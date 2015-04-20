\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{Referências}
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
\newcommand{\reff}[1]{\ensuremath{\texttt{ref}(#1)}}
\newcommand{\Reff}[1]{\ensuremath{\texttt{Ref}\:#1}}
\newcommand{\dereff}[1]{\ensuremath{\texttt{!}\:#1}}
\newcommand{\assign}[2]{\ensuremath{#1\:\texttt{:=}\:#2}}
\newcommand{\dom}[1]{\ensuremath{dom(#1)}}

%if False

> module Main where

%endif

\begin{document}
   \begin{frame}
      \titlepage
   \end{frame}
   \begin{frame}{Referências --- (I)}
      \begin{block}{Atribuições}
         \begin{itemize}
            \item Maioria das linguagens possui mecanismos para alterar valores previamente armazenados em uma região de memória.
            \item Em linguagens funcionais, os mecanismos para referência a valores e atribuições a estes são separados.
            \begin{itemize}
               \item Variáveis, assim que são associadas a um valor, este não pode ser modificado.
               \item O conceito convencional de atribuição é modelado usando referências (ou mônadas).
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (II)}
      \begin{block}{Referências}
         \begin{itemize}
            \item Uma referência é como uma variável, porém
            \begin{itemize}
               \item Possuem operações explícitas para atribuição e acesso a valores que estas possuem.
               \item Operações suportadas por referências: alocação, acesso e atribuição --- similares a ponteiros.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (III)}
      \begin{block}{Referências}
         \begin{spec}
r = ref 5; print (! r); r := 7 ; print (! r)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (IV)}
      \begin{block}{Aliasing}
         \begin{itemize}
            \item Considere o seguinte exemplo:
            \begin{spec}
r = ref 13 ; s = r ; s := 82 ; print (! r)
            \end{spec}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (V)}
      \begin{block}{Referências e ponteiros}
         \begin{itemize}
            \item Apesar de similares, referências não são equivalentes a ponteiros.
            \item Isso se deve ao fato de que referências não podem ser desalocadas.
            \begin{itemize}
               \item Uma operação de desalocar uma referência pode violar a propriedade de preservação de tipos.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (VI)}
      \begin{block}{Preservação de tipos}
         \begin{spec}
let r = ref 0
in s = r ;
free r;
let t = ref false
in t := true ;
succ (! s)
         \end{spec}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (VII)}
      \begin{block}{Semântica}
         \begin{itemize}
            \item Qual o resultado de se alocar uma referência?
            \item O que são os valores de um tipo $\Reff{\tau}$?
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (VIII)}
      \begin{block}{Alocação dinâmica}
         \begin{itemize}
            \item Como alocação dinâmica é implementada?
            \begin{itemize}
               \item RTS mantêm um registro do que está alocado e do que está livre.
               \item Ao alocarmos dinamicamente uma área de memória de um certo tipo, o
                     RTS aloca o espaço necessário e retorna o endereço de ínicio desta área.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (IX)}
      \begin{block}{Voltando a semântica...}
         \begin{itemize}
           \item Precisamos de ``controlar'' o que foi alocado ou livre durante a execução.
           \item Para isso, usaremos uma função finita $\mu$ de endereços $l$ para valores.
           \item Além disso, consideraremos que um endereço é um valor.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (X)}
      \begin{block}{Sintaxe de termos}
         \[
            \begin{array}{lcl}
               t & ::= & x \\
                 & \mid & \lambda x : \tau . t\\
                 & \mid & t \: t \\
                 & \mid & \unit \\
                 & \mid & \reff{t} \\
                 & \mid & \dereff{t} \\
                 & \mid & \assign{t}{t}\\
                 & \mid & l\\
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (XI)}
      \begin{block}{Semântica --- Valores}
         \[
            \begin{array}{lcl}
               v & ::= & \lambda x : \tau . t \\
                 & \mid & unit \\
                 & \mid & l \\
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (XII)}
      \begin{block}{Semântica}
         \[
             \begin{array}{c}
                \infer[_{(EAppAbs)}]{(\lambda x : \tau . t)\:v\:\mid\:\mu\Rightarrow [x \mapsto v] t\:\mid\:\mu}{} \\ \\
                \infer[_{(EApp_1)}]{t_1\:t_2\:\mid\:\mu\Rightarrow t_1\:t_2\:\mid\:\mu'}
                                   {t_1\:\mid\:\mu \Rightarrow t_1'\:\mid\mu'} \\ \\
                \infer[_{(EApp_2)}]{v_1\:t_2\:\mid\:\mu\Rightarrow v_1\:t_2'\:\mid\:\mu'}
                                   {t_2\:\mid\:\mu \Rightarrow t_2'\:\mid\mu'} \\ \\

             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (XIII)}
      \begin{block}{Semântica}
         \[
             \begin{array}{c}
                \infer[_{(ERefV)}]{\reff{v}\:\mid\:\mu\Rightarrow l \:\mid\: \mu , l\mapsto v}{l\not\in\dom{\mu}} \\ \\
                \infer[_{(ERef)}]{\reff{t_1} \:\mid\:\mu \Rightarrow \reff{t_1'}\:\mid\:\mu'}{t_1 \:\mid\:\mu \Rightarrow t_1'\:\mid\:\mu'} \\ \\
                \infer[_{(EDerefLoc)}]{\dereff{l}\:\mid\:\mu \Rightarrow v \:\mid\:\mu}{\mu(l) = v} \\ \\
                \infer[_{(EDeref)}]{\dereff{t_1} \:\mid\:\mu \Rightarrow \dereff{t_1'}\:\mid\:\mu'}{t_1 \:\mid\:\mu \Rightarrow t_1'\:\mid\:\mu'} \\ \\
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (XIV)}
      \begin{block}{Semântica}
         \[
             \begin{array}{c}
                \infer[_{(EAssign)}]{\assign{l}{v_2}\,\mid\,\mu\Rightarrow\,\unit\,\mid\,[l\mapsto v_2]\mu}{} \\ \\
                \infer[_{(EAssign_1)}]{\assign{t_1}{t_2}\,\mid\,\mu\Rightarrow\,\assign{t_1'}{t_2}\,\mid\,\mu'}{t_1 \:\mid\:\mu \Rightarrow t_1'\:\mid\:\mu'} \\ \\
                \infer[_{(EAssign_2)}]{\assign{v_1}{t_2}\,\mid\,\mu\Rightarrow\,\assign{v_1}{t_2'}\,\mid\,\mu'}{t_2 \:\mid\:\mu \Rightarrow t_2'\:\mid\:\mu'} \\ \\
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (XV)}
      \begin{block}{Tipagem}
         \begin{itemize}
            \item Mas, ainda resta uma questão: Como verificar tipos de referências?
            \item Idealmente, uma referência deve armazenar valores de um único tipo. Como fazer isso?
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Referências --- (XV)}
      \begin{block}{Tipagem}
         \begin{itemize}
            \item Mas, ainda resta uma questão: Como verificar tipos de referências?
            \item Idealmente, uma referência deve armazenar valores de um único tipo. Como fazer isso?
         \end{itemize}
      \end{block}
   \end{frame}
\end{document}
