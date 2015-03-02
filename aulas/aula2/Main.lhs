\documentclass{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[brazil]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage{proof}
\usepackage{color}
 
\usetheme{Luebeck}
 
\title{Introdução à Semântica Operacional}
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
   \begin{frame}{Semântica Operacional --- (I)}
       \begin{block}{Formalismos para Def. Semânticas}
         \begin{itemize}
            \item Semântica Denotacional.
            \item Semântica Axiomática.
            \item Semântica Operacional.
         \end{itemize}
       \end{block}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (II)}
       \begin{block}{Tipos de Semântica Operacional}
         \begin{itemize}
            \item Semântica big-step
            \item Semântica small-step
         \end{itemize}
       \end{block}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (III)}
       \begin{block}{Linguagem de exemplo --- Expressões aritméticas}
           \[
                \begin{array}{rcl}
                   e & ::= & n \\
                     & \mid & e + e\\
                \end{array}
           \]
       \end{block}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (IV)}
       \begin{block}{Semântica Operacional Big-step}
           \[
                \begin{array}{c}
                   \fbox{$e \Downarrow e'$} \\
                   \\
                   \begin{array}{c}
                      \infer[_{(\text{Const})}]
                            {n \Downarrow n}
                            {} \\ \\
                      \infer[_{(\text{Add})}]
                            {e + e' \Downarrow n \oplus n'}
                            {e \Downarrow n & e' \Downarrow n'}
                   \end{array}
                \end{array}
           \]
       \end{block}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (V)}
       \begin{block}{Semântica Operacional Small-step}
           \[
                \begin{array}{c}
                   \fbox{$e \to e'$} \\
                   \\
                   \begin{array}{c}
                       \infer[_{(Const)}]
                             {n + n' \to n \oplus n'}
                             {} \\ \\
                       \infer[_{(AddL)}]
                             {e + e' \to e_1 + e'}
                             {e \to e_1} \\ \\
                      \infer[_{(AddR)}]
                             {n + e' \to n + e_1'}
                             {e' \to e_1'}
                   \end{array}
                \end{array}
           \]
       \end{block}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (VI)}
       \begin{block}{Derivação de Exemplo: Semântica Big-step}
             \[
                \infer[_{(Add)}]
                      {(1 + 2) + 3 \Downarrow 6}
                      {
                        \infer[_{(Add)}]
                              {1 + 2 \Downarrow 3}
                              {
                                \infer[_{(Const)}]
                                      {1 \Downarrow 1}{}
                                &
                                \infer[_{(Const)}]
                                      {2 \Downarrow 2}{}
                              }
                        &
                        \infer[_{(Const)}]
                              {3 \Downarrow 3}
                              {}
                      }
             \]
       \end{block}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (VII)}
       \begin{block}{Derivação de Exemplo: Semântica Small-step}
           \[
                   \infer[_{(AddL)}]
                         {(1+2)+3 \to 3 + 3}
                         {
                            \infer[_{(Const)}]
                                  {1 + 2 \to 3}
                                  {}
                         }
           \]
       \end{block}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (VIII)}
       \begin{block}{Notação}
          \begin{itemize}
             \item $e \to^* e'$: $e$ produz $e'$ em zero ou mais passos. Formalmente:
             \[
                \begin{array}{l}
                    e \to^* e \\
                    \text{se }e \to^* e_1 \text{ e }e_1\to e' \text{ então }e\to^* e'
                \end{array}
             \]
             \item $e \to^* e'$ é o fechamento reflexivo e transitivo de $e\to e'$
          \end{itemize}
       \end{block}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (IX)}
       \begin{theorem}[Determinismo]
              Para todo $e$, $e'$ e $e''$, se $e \Downarrow e'$ e $e \Downarrow e''$ então $e = e''$.
       \end{theorem}

        \begin{theorem}[Equivalência]
                Para todo $e$ e $n$, temos que $e \Downarrow n$ se, e somente se $e \to^* n$
        \end{theorem}
   \end{frame}
   \begin{frame}{Semântica Operacional --- (X)}
      \begin{itemize}
          \item Sintaxe da linguagem
      \end{itemize}
 
> data Exp = Const Value | Exp :+: Exp
>            deriving (Eq, Ord)

> newtype Value = Value {unValue :: Int}
>                 deriving (Eq, Ord)
 
   \end{frame}
   \begin{frame}{Semântica Operacional --- (XI)}
      \begin{itemize}
        \item Interpretador ``Big-step''
      \end{itemize}
 
> bigStep :: Exp -> Value
> bigStep (Const v) = v
> bigStep (e :+: e') = sumVal (bigStep e) (bigStep e')
 
   \end{frame}
  \begin{frame}{Semântica Operacional --- (XII)}
     \begin{itemize}
        \item Interpretador ``Small-step''
     \end{itemize}
 
> smallStep :: Exp -> Either Exp Value
> smallStep (Const v) = Right v
> smallStep ((Const v) :+: (Const v')) = Right (sumVal v v')
> smallStep ((Const v) :+: e') =
>     let e = either id Const (smallStep e')
>     in smallStep ((Const v) :+: e)
> smallStep (e :+: e') =
>     let e1 = either id Const (smallStep e)
>         e2 = either id Const (smallStep e')
>     in smallStep (e1 :+: e2)
 
  \end{frame}

% end of slides, just code from now on...
%if False

> sumVal :: Value -> Value -> Value
> sumVal v v' = Value (unValue v + unValue v')
 
> lexer = Token.makeTokenParser haskellDef
> integer = Token.integer lexer

> pExp = chainl1 pConst pPlus

> pConst = (Const . Value . fromInteger <$> integer) <|> Token.parens lexer pExp

> pPlus = const (:+:) <$> Token.lexeme lexer (Token.symbol lexer "+")

> instance Show Value where
>    show = show . unValue

> instance Show Exp where
>    show (Const v) = show v
>    show (e1@(_ :+: _) :+: e2) = parens (show e1) ++ " + " ++ show e2
>    show (e :+: e') = show e ++ " + " ++ show e'

> parens s = "(" ++ s ++ ")"

> main :: IO ()
> main
>   = do
>      putStrLn "Interpretador de Expressões"
>      putStrLn "****************************"
>      putStr "Digite uma expressão:"
>      liftM (parse pExp "") getLine >>= interp

> interp :: Either ParseError Exp -> IO ()
> interp (Left err) = print err
> interp (Right e)
>    = do
>        putStr "\nEscolha o interpretador (1 - Bigstep, 2 - Small step):"
>        liftM (parse integer "") getLine >>= finish e

> finish :: Exp -> Either ParseError Integer -> IO ()
> finish _ (Left err) = print err
> finish e (Right n) = if n == 1 then print (bigStep e) else either print print (smallStep e)

%endif
\end{document}
