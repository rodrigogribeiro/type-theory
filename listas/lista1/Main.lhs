\documentclass[11pt,a4paper]{report}

\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
\usepackage{fullpage}
%\usepackage[linesnumbered, vlined]{algorithm2e}

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
%format `union` = "\V{\:\cup\:}"

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

> import Data.List
 
%endif

\begin{document}

 \hfill DECSI - UFOP \\
{\it Teoria de Tipos}
 \hfill $\mbox{1}^{\mbox{\underline{o}}}$ semestre de 2015 \\
Professor: \parbox[t]{14cm}{Rodrigo Geraldo Ribeiro \\
                     e-mail: rodrigo em decsi.ufop.br}\\

\noindent {\bf Lista de Exerc\'icios 1} \hfill {\bf Tema: Semântica Operacional e $\lambda$-Cálculo Atipado}

\vspace*{3mm}
\begin{enumerate}
  \item Considere a seguinte linguagem de expressões aritméticas cuja
    semântica é intuitivamente óbvia.
  \[
        \begin{array}{lcll}
             e & ::= & n & \text{números naturais}\\
               & \mid & \textit{true} &\text{constante true}\\
               & \mid & \textit{false}&\text{constante false}\\
               & \mid & \texttt{if } e \textit{ then }e\textit{ else
               }e & \text{teste condicional}\\
              &  \mid & e \texttt{ + } e &\text{soma de números} \\
              & \mid & e \texttt{ \& } e  & \text{conjunção}\\
              & \mid & \texttt{zero? }e  & \text{teste de igualdade a
              zero}.
        \end{array}
  \]
  \begin{enumerate}
      \item Apresente uma definição adequada do que seriam valores
        para esta linguagem.
      \item Apresente uma semântica operacional small step para
        esta linguagem.
     \item Apresente uma semântica operacional big step para esta
       linguagem.
     \item As semânticas apresentadas por você nos itens anteriores
       são determinísticas? Em caso positivo, prove este fato. Caso
       contrário, apresente uma semântica determinista para esta
       linguagem e prove este fato.
     \item Seja $e \to e'$ a definição da semântica small-step
       apresentada por você anteriormente. Denote por $e \to^* e'$ o
       fecho reflexivo e transitivo da relação de semântica
       small-step. É verdade que para todo $e$, $e \to^* v$, em que
       $v$ é um valor definido por você no item a)? Se sim, prove este
       fato. Caso contrário apresente um contra-exemplo e argumente se
       é possível ou não corrigir a linguagem de forma a garantir que
       $e \to^* v$, para qualquer $e$ e $v$.
     \item Implemente o interpretador para esta linguagem usando
       Haskell e permita que o usuário escolha entre utilizar a
       semântica small-step ou big-step definida por
       você. Evidentemente, você deverá implementar duas funções de
       interpretação: uma para cada semântica definida.
  \end{enumerate}
  \item Considerando a representação de números naturais e valores
    booleanos no $\lambda$-cálculo, faça o que se pede:
  \begin{enumerate}
      \item Defina a operação de disjunção $(\lor)$ em
        $\lambda$-cálculo.
      \item Defina a operação de multiplicação em $\lambda$-cálculo,
        sem utilizar a operação de adição.
  \end{enumerate}
  \item A seguir, apresento uma definição de um tipo para $\lambda$-termos que é parametrizado pelo tipo que representa variáveis.

> data Term a = Var a
>             | Lam a (Term a)
>             | App (Term a) (Term a)
>
> type STerm = Term String


   Sejam |size| e |fv| as seguintes funções sobre $\lambda$-termos:

> size :: Term a -> Int
> size (Var _) = 1
> size (Lam _ t) = 1 + size t
> size (App l r) = 1 + size l + size r


> fv :: Eq a => Term a -> [a]
> fv (Var v) = [v]
> fv (Lam v t) = fv t \\ [v]
> fv (App l r) = fv l `union` fv r

Prove que para todo |t :: Term a|, |length (fv t)| $\leq$ |size t|.
      \item Implemente um interpretador para o $\lambda$-cálculo atipado. Seu interpretador deve fornecer um prompt de
            comando que permita ao usuário entrar com $\lambda$-termos que serão interpretados utilizando índices DeBruijn.
            Evidentemente, seu interpretador deve apresentar como resultado uma forma normal, caso esta exista. Além disso,
            visando facilitar a legibilidade de seus resultados, ao exibí-los você deve utilizar nomes ao invés de índices DeBruijn.
      \item[\ ]
      \item[\ ]
      \item[\ ] Algumas observações importantes:
      \begin{enumerate}
          \item Seus resultados devem ser entregues como um arquivo .lhs que pode ser processado pelo pré processador lhs2\TeX~ para
                gerar um pdf contendo suas respostas. Como exemplo de código, utilize o fonte desta lista.
          \item O uso de makefile é obrigatório.
          \item Pode-se instalar o lhs2\TeX~ usando os seguintes comandos
\begin{center}
\verb|cabal update|\\
\verb|cabal install lhs2TeX|
\end{center}
          \item Prazo para entrega improrrogável: 27/03/2014.
      \end{enumerate}
   \end{enumerate}
\end{document}
