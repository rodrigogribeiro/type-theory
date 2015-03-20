\documentclass[11pt,a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage[pdftex]{hyperref}
\usepackage{color}
\usepackage{graphicx}
 

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

%if False

> module Main where
 
%endif

\begin{document}

 \hfill DECSI - UFOP \\
{\it Teoria de Tipos}
 \hfill $\mbox{1}^{\mbox{\underline{o}}}$ semestre de 2015 \\
Professor: \parbox[t]{14cm}{Rodrigo Geraldo Ribeiro \\
                     e-mail: rodrigo em decsi.ufop.br}\\

\noindent Lista de Exerc\'icios 2 \hfill Tema: Tipos simples e Elaboração de programas

\vspace*{3mm}

\section{Introdução}

Neste trabalho voc\^e ir\'a implementar um interpretador de uma linguagem imperativa simples denominada IMP. Para
isto, ser\'a necess\'ario implementar as diversas partes que comp\~oe o interpretador e formalizar sua semântica
e sistema de tipos.

\section{A linguagem IMP}

\subsection{Uma Introdu\c{c}\~ao a Linguagem IMP}\label{informal}

Programas IMP s\~ao extremamente simples e consistem apenas de uma lista de comandos que s\~ao executados
 sequ\^encialmente. Cada comando pode ser classificado como:
 \begin{itemize}
 	\item Comandos de entrada / sa\'ida: Existem apenas comandos para leitura de valores no teclado
 	      (comando \texttt{read}) e um comando para a impress\~ao de resultados (comando \texttt{print}).
 	      O comando \texttt{read} recebe como par\^ametro uma vari\'avel e associa a esta o valor lido a partir do
 	      teclado. Por sua vez, o comando \texttt{print} pode receber express\~oes arbitr\'arias como par\^ametro.
 	\item Atribui\c{c}\~ao de valores: Atribui\c{c}\~ao de valores possui o significado usual de linguagens
 	      imperativas, permitindo associar o valor de uma express\~ao \texttt{e} a uma vari\'avel \texttt{v}.
  	\item Comando condicional: Comandos da forma \texttt{if e then c1 else c2}, onde \texttt{e} \'e uma express\~ ao
  	      booleana e \texttt{c1}, \texttt{c2} s\~ao blocos de comandos. Blocos s\~ao delimitados por \texttt{``\{''}
  	      e \texttt{``\}''}.
 	\item Loop: Comandos da forma \texttt{while e do c}, onde \texttt{e} \'e uma express\~ ao
  	      booleana e \texttt{c} \'e um bloco de comandos.
  	\item Comando \texttt{skip}: Comando equivalente a uma opera\c{c}\~ao que n\~ao realiza nenhuma avalia\c{c}\~ao,
  	      nem efeito colateral. \'E um comando que n\~ao ``faz nada''.
 \end{itemize}

Antes de se apresentar a especifica\c{c}\~ao formal da linguagem IMP, \'e apresentado um programa de exemplo. Este
programa realiza o c\'alculo do fatorial de n\'umero lido a partir do teclado e ao final imprime o resultado do
 fatorial.

\begin{verbatim}
var {
  int x;
  int fatorial;
}
program {
  print "digite um numero:";
  read x;
  fatorial := 1;
  while ! (x <= 1) do {
    fatorial := fatorial * x;
    x := x - 1;
  }
  print fatorial;
}\end{verbatim}

O sem\^antica do trecho de c\'odigo anterior deve ser intuitiva para o leitor. O bloco \texttt{var} serve para definir
as vari\'aveis a serem utilizadas no programa e o bloco \texttt{program} consiste da defini\c{c}\~ao do programa em
si.

\subsection{Especifica\c{c}\~ao Sint\'atica da Linguagem IMP}\label{sintaxe}

Nesta se\c{c}\~ao \'e apresentada a gram\'atica da linguagem IMP. Nela adotamos as seguintes conven\c{c}\~oes:
\begin{itemize}
	\item Terminais: Tokens (tamb\'em chamados de terminais) s\~ao representados utilizando fonte \texttt{typewriter}.
	\item N\~ao Terminais: Vari\'aveis da gram\'atica (tamb\'em chamados de n\~ao terminais) s\~ao representados
	      utilizando fonte em \textit{it\'alico}.
\end{itemize}

A gram\'atica \'e apresentada na figura \ref{gramatica}.

\begin{figure}[ht]
	\begin{tabular}{rcl}
	\textit{compilation unit} & $\rightarrow$ & \textit{varblock} \textit{programblock}\\
        \textit{varblock}         & $\rightarrow$ & \texttt{var} \texttt{\{} \textit{var defs} \texttt{\}}\\
        \textit{var defs}         & $\rightarrow$ & \textit{var def} \textit{var defs}\\
                                  & $\mid$           & $\lambda$\\
        \textit{var def}          & $\rightarrow$ & \textit{type} \texttt{identifier} \texttt{;}\\
        \textit{type}             & $\rightarrow$ & \texttt{int} $\mid$ \texttt{bool} \\
        \textit{programblock}    & $\rightarrow$ & \texttt{program} \texttt{\{} \textit{commands} \texttt{\}}\\
        \textit{commands}         & $\rightarrow$ & \textit{command} \texttt{;} \textit{commands}\\
        						  & $\mid$          & $\lambda$\\
        \textit{command}          & $\rightarrow$ & \texttt{skip}\\
        						  & $\mid$           & \texttt{identifier} \texttt{:=} \textit{bexp}\\
        						  & $\mid$           & \texttt{print} \textit{bexp}\\
        						  & $\mid$			  & \texttt{read} \textit{identifier}\\
        						  & $\mid$           & \texttt{if} \textit{bexp} \texttt{then} \textit{block}
        						                      \texttt{else} \textit{block}\\
        						  & $\mid$           & \texttt{while} \textit{bexp} \texttt{do} \textit{block}\\
        \textit{block}            & $\rightarrow$ & \texttt{ \{ } \textit{commands} \texttt{\}}\\
        \textit{bexp}             & $\rightarrow$ & \texttt{true} $\mid$ \texttt{false}\\
                                  & $\mid$           & \textit{bexp} \texttt{=} \textit{bexp}\\
                                  & $\mid$           & \textit{bexp} \texttt{<=} \textit{bexp}\\
                                  & $\mid$           & \texttt{$!$} \textit{bexp}\\
                                  & $\mid$           & \textit{bexp} \texttt{\&} \textit{bexp}\\
                                  & $\mid$           & \textit{bexp} \texttt{$\mid$} \textit{bexp} \\
                                  & $\mid$           & \texttt{(} \textit{bexp} \texttt{)}\\
                                  & $\mid$           & \texttt{string}\\
                                  & $\mid$           & \textit{aexp}\\
        \textit{aexp}             & $\rightarrow$ & \texttt{number}\\
                                  & $\mid$           & \texttt{identifier}\\
                                  & $\mid$           & \textit{aexp} \texttt{+} \textit{aexp} \\
                                  & $\mid$           & \textit{aexp} \texttt{-} \textit{aexp} \\
                                  & $\mid$           & \textit{aexp} \texttt{*} \textit{aexp} \\
                                  & $\mid$           & \textit{aexp} \texttt{/} \textit{aexp} \\
                                  & $\mid$           & \texttt{(} \textit{aexp} \texttt{)}\\
	\end{tabular}
	\centering
	\caption{Gram\'atica Livre de Contexto de IMP}
	\label{gramatica}
\end{figure}

Com base no apresentado, faça o que se pede:

\begin{enumerate}
   \item Apresente uma semântica operacional de passo pequeno para a linguagem IMP. Como inspiração para sua definição você pode consultar o livro The Formal Semantics of Programming Languages --- An
         Introduction de Glynn Winskel ou o livro \href{http://www.cis.upenn.edu/~bcpierce/sf/current/index.html}{Software Foundations}, que aborda semântica formal e teoria de tipos usando o
         assistente de provas Coq.
   \item Apresente o sistema de tipos para a linguagem IMP. Mostre que o seu sistema de tipos é seguro provando teoremas de progresso e preservação.
   \item Implemente o interpretador e o algoritmo de verificação de tipos para IMP em Haskell.
   \item A linguagem IMP não possui comandos \texttt{for}, \texttt{do ... while} e nem \texttt{if ... then} (sem else) presentes na maioria das linguagens imperativas.
   \begin{enumerate}
       \item Apresente regras de semântica e do sistema de tipos para cada uma destas novas construções. Estenda a prova de preservação e progresso para estas construções.
       \item Mostre como estas construções podem ser elaboradas para a sintaxe de IMP apresentada na figura \ref{gramatica}.
       \item Modifique seu interpretador de forma que este considere as novas construções de IMP e elabore-as para o núcleo desta linguagem. Implemente o processo de elaboração como
             parte do algoritmo de verificação de tipos.
   \end{enumerate}
     \item[\ ]
     \item[\ ]
      \item[\ ] Algumas observações importantes:
      \begin{enumerate}
          \item Como o código a ser produzido como parte deste trabalho é bem mais extenso que o anterior, recomenda-se a divisão da implementação em diferentes módulos.
          \item O relatório a ser produzido deverá utilizar lhs2\TeX~ e deverá possuir detalhes de sua implementação e formalização.
          \item O uso de makefile para produção do relatório é obrigatório.
          \item Prazo para entrega improrrogável: 17/04/2014.
      \end{enumerate}
\end{enumerate}

\end{document}
