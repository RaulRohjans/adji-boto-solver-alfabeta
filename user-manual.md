# Manual do Utilizador

## Introdução
O projeto Adji-Boto é uma aplicação desenvolvida em Common Lisp que implementa o algoritmo de Alpha-Beta Pruning para resolver problemas relacionados ao jogo Oware. Este manual tem como objetivo orientar o utilizador na utilização do programa, desde a sua instalação até à execução e interpretação dos resultados.

## Requisitos do Sistema
Para executar o Adji-Boto, são necessários os seguintes requisitos:
- Sistema Operativo: Linux ou Windows.
- Lisp Environment: Common Lisp com suporte a Quicklisp.


## Como Utilizar o Programa

### 1. Iniciar o Programa
- Abra o terminal e execute o ambiente Lisp.
- Carregue o ficheiro `jogo.lisp` para inciar o projeto.

### 2. Menu Principal
- O menu principal apresenta as seguintes opções:
  1. Resolver Problema: Permite selecionar um dos problemas disponíveis para resolução.
  2. Visualizar Problemas: Mostra os problemas de jogo do ficheiro `problemas.dat`.
  3. Sair: Termina a execução do programa.

### 3. Resolução de Problemas
- Selecione a opção Resolver Problema.
- Escolha o problema a ser resolvido (A, B, C, D, E, F ou G).
- Defina a profundidade da pesquisa, tendo em conta que profundidades elevadas podem causar overflow em problemas complexos.
- A opção de algoritmo MinMax com AlfaBeta permite iniciar o processo de resolução automaticamente.

### 4. Visualizar os Resultados
- O resultado da resolução do problema é exibido no terminal e registado no ficheiro `log.dat`.
- O log inclui informações detalhadas sobre o estado final, nós analisados, cortes alfa e beta e tempo de execução.

## Limitações Conhecidas
- Overflow pode ocorrer em problemas complexos (E e F) devido à profundidade da pesquisa.
- A interface é baseada em terminal, sem suporte gráfico.
- Não foi implementada uma limitação de tempo para as jogadas.
