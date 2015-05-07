\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
\usepackage{stmaryrd}
\usepackage{listings}

\lstset{language = Java}
 
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
\newcommand{\intt}[0]{\ensuremath{\texttt{Int}}}
\newcommand{\floatt}[0]{\ensuremath{\texttt{Float}}}
\newcommand{\arrayy}[1]{\ensuremath{\texttt{Array }#1}}

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
   \begin{frame}{Subtipagem --- (V)}
     \begin{block}{Relação de subtipagem}
        \[
           \begin{array}{c}
              \infer[_{(IntFloat)}]{\subb{\intt}{\floatt}}{}
           \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (VI)}
     \begin{block}{Registros}
        \begin{itemize}
           \item As regras de subtipagem, são bem triviais, de um ponto de vista teórico.
           \item A subtipagem fica mais interessante ao considerarmos registros.
           \begin{itemize}
               \item Registros servem como preparação para formalização de linguagens O.O. como Java.
           \end{itemize}
        \end{itemize}
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (VII)}
     \begin{block}{Registros --- sintaxe}
        \[
            \begin{array}{lcl}
               t & ::= & ... \\
                 & \mid & \{l_i = t_i\}^{i = 1..n} \\
                 & \mid & t.l \\
               v & ::= & ... \\
                 & \mid & {l_i = v_i}^{i = 1..n} \\
            \tau & ::= & ... \\
                 & \mid & \{l_i : \tau_i\}^{i = 1..n}\\
            \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (VIII)}
     \begin{block}{Registros --- semântica}
        \[
           \begin{array}{cc}
              \infer[_{(EProjRcd)}]{\{l_i = v_i\}^{i = 1..n}.l_j \Rightarrow v_j} &
              \infer[_{(EProj)}]{t_1.l \Rightarrow t_1'.l}{t_1 \Rightarrow t_1'} \\ \\
              \multicolumn{2}{c}{\infer[_{(ERcd)}]{\{...,l_j = t_j,...\}\Rightarrow\{...,l_j = t_j',...\}}{t_j\Rightarrow t_j'}}
           \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (IX)}
     \begin{block}{Registros --- sistema de tipos}
        \[
           \begin{array}{c}
              \infer[_{(TRcd)}]{\Gamma\vdash\{ l_i : \tau_i\}^{i=1..n}}{\forall i.i\in\{1..n\} \to \Gamma\vdash t_i : \tau_i} \\ \\
              \infer[_{(TProj)}]{\Gamma\vdash\{ l_i : \tau_i\}^{i=1..n}.t_j : \tau_j}{\Gamma\vdash\{ l_i : \tau_i\}^{i=1..n} : \{l_i : \tau_i\}^{i = 1..n}}
           \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (X)}
      \begin{block}{Relação de subtipagem para registros}
         \[
             \begin{array}{c}
                \infer[_{(SRecWidth)}]{\subb{\{l_i : \tau_i\}^{i = 1..n+k}}{\{l_i : \tau_i\}^{i = 1..n}}}{} \\ \\
                \infer[_{(SRecDepth)}]{\subb{\{l_i : \tau_i\}^{i = 1..n}}{\{l_i : \tau_i'\}^{i = 1..n}}}{\subb{\tau_i}{\tau_i'}^{i = 1..n}} \\ \\
                \infer[_{(SRecPerm)}]{\subb{\{k_i : \tau_i\}^{i = 1..n}}{\{l_j : \tau_j\}^{j = 1..n}}}{\{k_i : \tau_i\}^{i = 1..n}\text{ \'e uma permutação de }\{l_j : \tau_j\}^{j = 1..n}}
             \end{array}
         \]
      \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XI)}
      \begin{block}{Subtipagem e Anotações de tipos}
         \begin{itemize}
            \item Anotações, em linguagens sem subtipagem, são semanticamente inócuas.
            \item Em linguagens com subtipagem, a situação é mais interessante.
            \begin{itemize}
               \item Permite \emph{up-casts} e \emph{down-casts}.
               \item Up-cast: Coerção para um super-tipo (verificável).
               \item Down-cast: Coerção insegura. Tentar anotar um valor com um subtipo de seu tipo real.
                                Porém, isso pode acarretar erros em tempo de execução --- Em Java: ClassCastException.
            \end{itemize}
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XII)}
      \begin{block}{Progresso e Down-casts}
         \begin{itemize}
            \item Down-casts podem comprometer o progresso.
            \[
                 \infer[_{(EDownCast)}]{v_1 : \tau \Rightarrow v_1}{\vdash v_1 : \tau' & \subb{\tau}{\tau'}}
            \]
            \item Exemplo:
            \[
               \begin{array}{cc}
                  \lambda x : \top. (x : \{a : \Nat\}).a & \{a = 5 , b = true\}
               \end{array}
            \]
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XIII)}
      \begin{block}{Type case}
         \begin{itemize}
            \item Operador de teste de tipo dinâmico (instanceof em Java)
            \item Muito usado em versões antigas da linguagem, antes desta incorporar polimorfismo paramétrico.
            \item Pode funcionar como uma possível solução para reobter progresso.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XIV)}
      \begin{block}{Subtipagem e Referências}
         \begin{itemize}
            \item Referências devem ser invariantes:
            \[
                \infer[_{(SRef)}]{\subb{\Reff{\tau_1}}{\Reff{\tau_2}}}{\subb{\tau_1}{\tau_2} & \subb{\tau_2}{\tau_1}}
            \]
            \item Subtipagem (registros) pode causar problemas com leitura e escrita de referências.
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XV)}
      \begin{block}{Subtipagem e arrays}
         \begin{itemize}
            \item Regra ideal, é similar a referências.
            \[
                \infer[_{(SArray)}]{\subb{\arrayy{\tau_1}}{\arrayy{\tau_2}}}{\subb{\tau_1}{\tau_2} & \subb{\tau_2}{\tau_1}}
            \]
            \item Java permite a seguinte regra mais liberal:
            \item Regra ideal, é similar a referências.
            \[
                \infer[_{(SArray)}]{\subb{\arrayy{\tau_1}}{\arrayy{\tau_2}}}{\subb{\tau_1}{\tau_2}}
            \]
         \end{itemize}
      \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XVI)}
     \begin{block}{Why Java arrays sucks!}
       \begin{flushleft}
        Object[] s = new Integer[4];\\
        s[0] = 4.4;
      \end{flushleft}
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XVII)}
     \begin{block}{Semântica de coerção}
        \begin{itemize}
           \item Traduzir termos de uma linguagem com subtipagem para uma sem.
           \item Formalmente, a tradução consiste de 3 funções:
           \begin{itemize}
              \item Uma para traduzir tipos.
              \item Uma para traduzir derivações de subtipagem.
              \item Uma para traduzir derivações de tipos.
           \end{itemize}
        \end{itemize}
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XVIII)}
     \begin{block}{Traduzindo tipos}
        \[
           \begin{array}{lcl}
             \llbracket \top \rrbracket & = & \Unit\\
             \llbracket \tau_1\to\tau_2 \rrbracket & = & \llbracket \tau_1 \rrbracket \to \llbracket \tau_2 \rrbracket \\
             \llbracket \{l_i : \tau_i\} \rrbracket & = & \{l_i : \llbracket \tau_i \rrbracket \}\\
           \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XIX)}
     \begin{block}{Traduzindo subtipagem}
        \[
           \begin{array}{lcl}
             \left\llbracket \infer[_{(SRefl)}]{\subb{\tau}{\tau}}{} \right\rrbracket & = & \lambda x : \llbracket \tau \rrbracket . x \\
             \\
             \left\llbracket \infer[_{(STop)}]{\subb{\tau}{\top}}{} \right\rrbracket & = & \lambda x : \llbracket \tau \rrbracket . unit \\
             \\
             \left\llbracket \infer[_{(STrans)}]{\subb{\tau_1}{\tau_3}}{\dfrac{C_1}{\subb{\tau_1}{\tau_2}} & \dfrac{C_2}{\subb{\tau_2}{\tau_3}}} \right\rrbracket & = &
                 \lambda x : \llbracket \tau \rrbracket . \llbracket C_2 \rrbracket (\llbracket C_1 \rrbracket x)\\
           \end{array}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XX)}
     \begin{block}{Traduzindo subtipagem}
        \[
          \footnotesize{
           \begin{array}{lcl}
             \left\llbracket \infer[_{(SArrow)}]{\subb{\tau_1\to\tau_2}{\tau_1'\to\tau_2'}}
                                                {\dfrac{C_1}{\subb{\tau_1'}{\tau_1}} & \dfrac{C_2}{\subb{\tau_2}{\tau_2'}}}
             \right\rrbracket & = & \lambda f : \llbracket \tau_1\to\tau_2 \rrbracket.\lambda x : \llbracket \tau_1' \rrbracket . \llbracket C_2 \rrbracket (f (\llbracket C_1 \rrbracket x)) \\
             \\
           \end{array} }
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XXI)}
     \begin{block}{Traduzindo subtipagem}
        \[
          \tiny{
           \begin{array}{lcl}
             \left\llbracket \infer[_{(SRecWidth)}]{\subb{\{l_i : \tau_i\}^{i \in 1.. n + k}}{\{l_i : \tau_i\}^{i \in 1.. n}}}{}\right\rrbracket & = &
                 \lambda r : \{l_i : \tau_i\}^{i \in 1.. n + k}.\{l_i = r . l_i^{i = 1..n}\} \\ \\
 
           \end{array}}
        \]
     \end{block}
   \end{frame}
   \begin{frame}{Subtipagem --- (XXII)}
   \end{frame}
\end{document}
