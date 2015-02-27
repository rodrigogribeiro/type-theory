%Implementando um interpretador de brainfuck
%Teoria de Tipos
%Professor Rodrigo Ribeiro

Interpretador --- (I)
=====================

- Motivação
     - Revisão de programação funcional
     - Análise sintática usando Parsec
     - Intuição sobre semântica operacional
- Configurações iniciais...

> module Main where

> import Text.ParserCombinators.Parsec


Interpretador --- (II)
======================

- Brainfuck
    - Turing tarpit language
    - Somente 8 comandos
    - Simples interpretação


Interpretador --- (III)
=======================

- Comandos brainfuck.
 
\begin{tabular}{|c|l|}
    \hline
    Caracter   & Semântica \\ \hline
    \texttt{>} & incrementa o ponteiro de dados \\
    \texttt{<} & decrementa o ponteiro de dados \\
    \texttt{+} & incrementa o byte sobre o ponteiro de dados \\
    \texttt{-} & decrementa o byte sobre o ponteiro de dados \\
    \texttt{.} & imprime o byte sobre o ponteiro de dados \\
    \texttt{,} & lê da entrada padrão um byte e o armazena na pos. atual\\
    \texttt{[} & Se o dado sobre o ptr = zero, move até \texttt{]}, move dir. \\
    \texttt{]} & Se o dado sobre o ptr != zero, move até \texttt{]},  move  esq.\\ \hline
\end{tabular}


Interpretador --- (IV)
======================

- "Estado" de um programa brainfuck
     - Lista infinita de bytes
     - Ponteiro de dados


Interpretador --- (V)
=====================

- Matemáticamente, o estado de um programa nada mais é que uma palavra e um posição desta,
  isto é: $\langle w , i \rangle$, em que $w$ é uma sequência infinita de bytes e $i$ a
  posição atual.
- Outra maneira equivalente: $\langle l\underline{x}r\rangle$, em que:
    - $l$: sequência de bytes a esquerda do ponteiro
    - $x$: byte sobre o ponteiro
    - $r$: sequência de bytes a direita do ponteiro
 
Intepretador --- (VI)
=====================
 
- Por simplicidade, usaremos o tipo Int de Haskell
  para bytes.

> type Byte = Int
> type Bytes = [Byte]

Interpretador --- (VII)
=====================

- Representando a configuração de um programa

> data Conf = Conf {
>                left    :: Bytes, -- esquerda de ptr
>                current :: Byte,  -- ptr
>                right   :: Bytes  -- direita de ptr
>             }

Interpretador --- (VIII)
======================

- Configuração inicial de um programa

> initial :: Conf
> initial = Conf {
>              left    = zeros,
>              current = 0,
>              right   = zeros
>          }

> zeros :: Bytes
> zeros = repeat 0
