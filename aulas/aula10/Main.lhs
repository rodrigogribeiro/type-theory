\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{Subtipagem}
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

\newcommand{\subb}[2]{\ensuremath{#1\,<:\,#2}}

%if False

> module Main where

%endif

\begin{document}
   \begin{frame}
      \titlepage
   \end{frame}
   \begin{frame}{Subtipagem --- (I)}
      \begin{block}{Subtipagem}
         \begin{itemize}
            \item Recurso presente em linguagens O.O.
            \item Conhecido como polimorfismo de subtipagem ou de inclusão.
            \item Primeiro recurso de linguagens não trivial.
            \begin{itemize}
               \item Possui interação complexa com outros recursos de linguagens.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (II)}
      \begin{block}{Motivação}
         \begin{itemize}
            \item A regra de tipagem de aplicações é irritante!
            \[
                \infer[_{(TApp)}]{\Gamma\vdash t\:t' : \tau}{\Gamma \vdash t : \tau' \to \tau & \Gamma \vdash t' : \tau'}
            \]
            \item Esta regra rejeitaria passarmos um inteiro para uma função esperando um float.
            \begin{itemize}
               \item A regra exige que os tipos sejam exatamente iguais!
               \item Muito restritiva em aplicações práticas.
            \end{itemize}
            \item Porém, como resolver este problema?
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (III)}
     \begin{block}{Solução}
        \begin{itemize}
           \item Formalizar, quando é seguro um tipo ser substituído por outro.
           \item Formalizamos isso usando a relação de subtipos, $\subb{\tau}{\tau'}$.
           \item Usamos uma regra para conectar a relação de tipagem e de subtipagem:
           \[
              \infer[_{(TSub)}]{\Gamma \vdash t : \tau}{\Gamma \vdash t : \tau' & \subb{\tau'}{\tau}}
           \]
        \end{itemize}
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (IV)}
     \begin{block}{Relação de subtipos}
        \[
           \begin{array}{cc}
              \infer[_{(SRefl)}]{\subb{\tau}{\tau}}{} & \infer[_{(STrans)}]{\subb{\tau}{\tau''}}{\subb{\tau}{\tau'} & \subb{\tau'}{\tau''}} \\ \\
              \infer[_{(SArrow)}]{\subb{\tau_1 \to \tau_2}{\tau_1' \to \tau_2'}}{\subb{\tau_1'}{\tau_1} & \subb{\tau_2}{\tau_2'}} &
              \infer[_{(STop)}]{\subb{\tau}{\top}}{}
           \end{array}
        \]
     \end{block}
   \end{frame}
\end{document}
