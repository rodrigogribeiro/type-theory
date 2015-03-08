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
\end{document}
