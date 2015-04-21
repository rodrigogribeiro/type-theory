\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{Exceções}
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

\newcommand{\err}[0]{\texttt{error }}
\newcommand{\tryy}[2]{\ensuremath{\texttt{try }#1\texttt{ with }#2}}
\newcommand{\raisee}[1]{\ensuremath{\texttt{raise }#1}}

%if False

> module Main where

%endif

\begin{document}
   \begin{frame}
      \titlepage
   \end{frame}
   \begin{frame}{Exceções --- (I)}
      \begin{block}{O que são exceções?}
         \begin{itemize}
            \item Transferência de controle não local ocasionada por uma situação não esperada durante a execução de um programa.
            \item Desafio: formalizar a semântica da transferência de controle.
            \begin{itemize}
               \item Desempilhar registros de ativação até chegar a um manipulador de exceções ou até que o programa seja encerrado.
               \item Semântica ``propaga'' erros recursivamente.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (II)}
      \begin{block}{O que apresentaremos?}
         \begin{itemize}
            \item Dois designs para lidar com exceções.
            \begin{itemize}
              \item O primeiro exceções simplesmente interrompem a execução sem retornar um valor.
              \item O segundo exceções retornam valores que permitem explicar o que ocasionou a exceção.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (III)}
      \begin{block}{Exceções sem valores --- sintaxe}
         \[
            \begin{array}{lcl}
               t & ::= & ... \\
                 & \mid & error \\
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (IV)}
      \begin{block}{Exceções sem valores --- semântica}
         \[
            \begin{array}{c}
               \infer[_{(EAppErr_1)}]{\err t_2 \Rightarrow \err}{} \\ \\
               \infer[_{(EAppErr_2)}]{v_1\:\err \Rightarrow \err}{}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (V)}
      \begin{block}{Exceções sem valores --- sistema de tipos}
         \[
            \begin{array}{c}
               \infer[_{(TError)}]{\Gamma\vdash\err : \tau}{}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (VI)}
      \begin{block}{Peculiaridades}
         \begin{itemize}
            \item A semântica aborta a execução tão logo um valor \err apareça.
            \item O termo \err possui qualquer tipo.
            \begin{itemize}
               \item Caso o sistema possua polimorfismo paramétrico, \err pode ter o tipo $\forall \alpha. \alpha$.
               \item Caso o sistema possua subtipagem, \err pode ter o tipo $\bot$, que é subtipo de todo tipo $\tau$.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (VII)}
      \begin{block}{Tratamento de exceções --- sintaxe}
         \[
            \begin{array}{lcl}
               t & ::= & ... \\
                 & \mid & \tryy{t}{t}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (VIII)}
      \begin{block}{Semântica}
        \[
            \begin{array}{c}
            \infer[_{(ETryV)}]{\tryy{v_1}{t_2}\Rightarrow v_1}{} \\ \\
            \infer[_{(ETryErr)}]{\tryy{\err}{t_2}\Rightarrow t_2}{} \\ \\
            \infer[_{(ETry)}]{\tryy{t_1}{t_2}\Rightarrow\tryy{t_1'}{t_2}}{t_1\Rightarrow t_1'}
            \end{array}
        \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (IX)}
      \begin{block}{Sistema de tipos}
         \[
            \begin{array}{c}
               \infer[_{(TTry)}]{\Gamma\vdash\tryy{t_1}{t_2}}{\Gamma\vdash t_1 : \tau & \Gamma \vdash t_2 : \tau}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (X)}
      \begin{block}{Alguns problemas com este design}
         \begin{itemize}
            \item Estas exceções não retornam valores, logo, fica difícil ``identificar'' o que aconteceu.
            \item Ideal: Assim como Java e C, exceções podem retornar valores que ajudam a
                  descobrir o que ocorreu ou a dar melhores mensagens de erro.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (XI)}
      \begin{block}{Exceções com valores --- sintaxe}
         \[
            \begin{array}{lcl}
                t & ::= & ...\\
                  & \mid & \tryy{t}{t}\\
                  & \mid & \raisee{t}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (XII)}
      \begin{block}{Exceções com valores --- semântica}
         \[
            \begin{array}{c}
               \infer[_{(EAppRaise_1)}]{(\raisee{v})\,t \Rightarrow \raisee{v}}{} \\ \\
               \infer[_{(EAppRaise_2)}]{v\,(\raisee{v'}) \Rightarrow \raisee{v'}}{} \\ \\
               \infer[_{(ERaise)}]{\raisee{t}\Rightarrow\raisee{t'}}{t\Rightarrow t'} \\ \\
               \infer[_{(ERaiseRaise)}]{\raisee{(\raisee{v})} \Rightarrow \raisee{v}}{}
            \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (XIII)}
      \begin{block}{Semântica}
        \[
            \begin{array}{c}
            \infer[_{(ETryV)}]{\tryy{v_1}{t_2}\Rightarrow v_1}{} \\ \\
            \infer[_{(ETryRaise)}]{\tryy{\raisee{v}}{t_2}\Rightarrow t_2\: v}{} \\ \\
            \infer[_{(ETry)}]{\tryy{t_1}{t_2}\Rightarrow\tryy{t_1'}{t_2}}{t_1\Rightarrow t_1'}
            \end{array}
        \]
      \end{block}
   \end{frame}
   \begin{frame}{Exceções --- (XIV)}
      \begin{block}{Sistema de tipos}
         \[
            \begin{array}{c}
               \infer[_{(TRaise)}]{\Gamma \vdash \raisee{t} : \tau}{\Gamma \vdash t : \tau_{err}} \\ \\
               \infer[_{(TTry)}]{\Gamma\vdash \tryy{t_1}{t_2} : \tau}{\Gamma \vdash t_1 : \tau & \Gamma \vdash t_2 : \tau_{err}\to \tau}
            \end{array}
         \]
      \end{block}
   \end{frame}
\end{document}
