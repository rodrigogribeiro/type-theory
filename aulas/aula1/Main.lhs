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

> import Data.Int (Int8)
> import Control.Applicative hiding (many, (<|>))
> import Control.Monad.State
> import Control.Monad.Trans
> import Control.Monad.Writer
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

Interpretador --- (VI)
======================

- Estrutura do interpretador
    - Análise sintática
    - Execução
- Análise sintática usando Parsec

Interpretador --- (VII)
========================

- Biblioteca de Parsing usando combinadores.
     - Funções primitivas de análise
     - Funções para combinar.

Interpretador --- (VIII)
=========================
 
- Caracteres considerados para parsing: < > . + - [ ] ,
- Verificação sintática: Todo "[" possui "]" correspondente
- Resultado do parsing: árvore sintática

Interpretador --- (IX)
======================

- Estrutura sintática

> data Cmd = ML | MR | Inc | Dec
>          | Pr | Rd | LL  | LR
>          deriving (Eq, Ord, Show)

> type Program = [Cmd]


Interpretador --- (X)
=====================

- Análise sintática

> pProgram :: Parser Program
> pProgram = nop *> many (between nop nop pins)

> nop :: Parser String
> nop = many (noneOf ops) <?> "nop"


Interpretador --- (XI)
======================

-- Análise sintática

> pins :: Parser Cmd
> pins = choice list

> list = map f table
>        where
>          f (c,t) = const t <$> char c

> table = zip ops [ML, MR,Inc,Dec,Pr,Rd,LL,LR]

> ops :: String
> ops = "><+-.,[]"


Intepretador --- (XII)
=====================

- Por simplicidade, usaremos o tipo Int de Haskell
  para bytes.

> type Byte = Int8
> type Bytes = [Byte]

Interpretador --- (XIII)
========================

- Representando a configuração da máquina
    - Posição atual da fita de bytes
    - Instrução atual do programa

Interpretador --- (XIV)
========================

- Configurações

> data Conf a = Conf {
>                left    :: [a], -- esquerda de ptr
>                current :: a,   -- ptr
>                right   :: [a]  -- direita de ptr
>             }
 
Interpretador --- (XV)
========================

- Configuração da fita

> type Tape = Conf Byte

- Posição do programa

> type ProgState = Conf Cmd

 
Interpretador --- (XVI)
=======================
 
- Configuração inicial de um programa

> initial :: Tape
> initial = Conf {
>              left    = zeros,
>              current = 0,
>              right   = zeros
>          }

> zeros :: Bytes
> zeros = repeat 0


Interpretador --- (XVII)
======================

- Mônada para execução de programas.
     - Mônada para logging de comandos executados.
     - Estado armazenado pela mônada State.

> type BF a = WriterT [Cmd] (StateT Tape IO) a


Interpretador --- (XVIII)
=========================

- Executando comandos individuais

> moveLeft :: Tape -> Tape
> moveLeft (Conf [] _ [])         = error "Impossible!"
> moveLeft (Conf (l:ls) b (r:rs)) = Conf ls l (b : r : rs)

> moveRight :: Tape -> Tape
> moveRight (Conf [] _ [])         = error "Impossible!"
> moveRight (Conf (l:ls) b (r:rs)) = Conf (b:l:ls) r rs


Interpretador --- (XIX)
=======================

- Executando comandos individuais

> incr :: Tape -> Tape
> incr (Conf ls b rs) = Conf ls (b+1) rs

> decr :: Tape -> Tape
> decr (Conf ls b rs) = Conf ls (b - 1) rs

Interpretador --- (XX)
========================

- Executando comandos individuais

> iprint :: Tape -> BF ()
> iprint = liftIO . print . current

> iread :: Tape -> BF ()
> iread (Conf ls _ rs)
>         = do
>             s <- liftIO getLine
>             put (Conf ls (read s) rs)


Interpretador --- (XX)
=======================

- Executando comandos individuais

> loopLeft :: Byte -> ProgState -> BF ProgState
> loopLeft b e@(Conf lp LL rp)
>             = if b == 0 then moveUntil LR e
>                 else nextCmd e
> loopLeft b (Conf _  _ _ ) = error "Impossible!"


Interpretador --- (XXI)
=======================

- Executando comandos individuais

> loopRight :: Byte -> ProgState -> BF ProgState
> loopRight b e@(Conf lp LR rp)
>             = if b /= 0 then moveUntil LL e
>                 else nextCmd e
> loopRight b (Conf _  _ _ ) = error "Impossible!"


Interpretador --- (XXII)
=========================

- Executando comandos individuais

> nextCmd :: ProgState -> BF ProgState
> nextCmd e@(Conf lp c []) = return e
> nextCmd (Conf lp c (r:rp)) = return (Conf (c:lp) r rp)

 
Interpretador --- (XXIII)
=========================

- Executar comandos individuais

> moveUntil :: Cmd -> ProgState -> BF ProgState
> moveUntil LL c@(Conf [] _ _) = return c
> moveUntil LL (Conf (l:lp) c rp)
>        = if l == LL then return (Conf lp LL (c : rp))
>            else moveUntil LL (Conf lp LL (c:rp))
> moveUntil LR c@(Conf [] _ _) = return c
> moveUntil LR (Conf lp c (r:rp))
>        = if r == LR then return (Conf (c:lp) r rp)
>            else moveUntil LR (Conf (c:lp) r rp)


Interpretador --- (XXIV)
========================

- Executando programas
