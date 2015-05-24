Lista 4 - Estendendo a Inferência de tipos para Core-ML 
======================================


Introdução
---------

Nas últimas aulas, foram apresentados o sistema de tipos para Core-ML
e como um algoritmo de inferência para esta linguagem pode ser
implementado em duas fases: geração de restrições e sua respectiva
solução. Argumentamos, informalmente que esta estrutura de
codificação permite a extensão da linguagem de maneira simples, visto
que, na maioria dos casos, estender a linguagem significa aumentar o
número de categorias sintáticas desta, o que implica apenas a
modificação de seu algoritmo de geração de restrições. E, em apenas
algumas situações específicas, o algoritmo de solução de restrições
necessita ser modificado.

Nesta lista de exercícios, iremos modificar o algoritmo de inferência
implementado adicionando duas extensões: 1) Produtos e operações sobre
estes. Esta é uma modificação simples e mostrará que a separação de
geração de restrições e solução de restrições permite a extensão
modular da linguagem. 2) Anotações de tipo. Apesar de uma extensão
simples, anotações demandam uma modificação na sintaxe de restrições
e em seu resolvedor.

As próximas seções descrevem cada uma destas tarefas e como estas
deverão ser incorporadas à implementação.

Produtos
--------

A adição de produtos a ML é simples, uma vez aue esta não implica em
alterações no resolvedor de constraints. Apenas modificações à sintaxe
de termos e as respectivas equações no gerador de restrições.

Anotações de tipos
----------------

Como o algoritmo de inferência de tipos para CoreML é correto e
completo, não há necessidade do programador anotar o tipo de funções,
visto que, o tipo destas pode ser calculado automaticamente pelo
compilador.

Porém, considera-se uma boa prática anotar o tipo de funções, já que
anotações de tipos pode ser vistas como uma forma de documentação
verificada pelo compilador.

** Modificações na sintaxe **

Por questões de simplicidade, permitiremos que anotações ocorram
apenas em definições de funções. Desta forma, alteraremos a sintaxe de
declarações para:

\[
\begin{array}{lcl}
decl & ::=    & x\,\,::\,\,\sigma \\
        & \mid & x = t
\end{array}
\]

em que $x$ é uma variável, $t$ um termo e $\sigma$ um tipo. Note, que
a sintaxe de declarações permite a existência de funções sem anotação
de tipo. Porém, evidentemente, não é possível existir uma anotação de
tipo, sem a função correspondente.

** Modificações no algoritmo de inferência **

A adição de anotações de tipo introduz um pequeno complicador sobre o
algoritmo de inferência de tipos: Como verificar que uma determinada
anotação é ou não válida? Uma possível idéia seria simplesmente exigir
que o tipo anotado seja "igual"[^1] ao tipo inferido porém, isso é
muito restritivo. O uso desta estratégia não permite a seguinte
definição:

> idInt :: Int -> Int
> idInt = \x -> x

Evidentemente, sem a anotação esta função possui como tipo inferido:

> idInt :: forall a. a -> a

Logo, é mais adequado permitirmos a anotação de tipos a funções desde
que estes sejam _instâncias_ do tipo inferido. Dizemos que um tipo $\sigma'$ é
instância de $\sigma$ se existe uma substituição $S$ tal que
$S(\sigma) = \sigma'$. Portanto, para verificar que uma anotação de
tipo é válida, basta verificar se o tipo anotado é uma instância do
tipo inferido.

Mas, a linguagem de constraints utilizada para inferir tipos para
coreML possui apenas um tipo de constraint: o de igualdade. Para
permitir o uso de anotações de tipos, devemos estender a sintaxe de
constraints para permitir o que chamaremos de constraints de
instanciação, representados por $\sigma \prec \sigma'$. Tal constraint
representa que $\sigma'$ é uma instância de $\sigma'$.

Para permitir anotações de tipos, deveremos usar a seguinte sintaxe de
constraints:

\[
\begin{array}{lcl}
C & ::= & \sigma \equiv \sigma \\
   & \mid & \sigma \prec \sigma
\end{array}
\]

Evidentemente, a modificação da linguagem de constraints gera uma
modificação na linguagem do

[^1]: Igual a menos de renomeamento de variáveis. 

