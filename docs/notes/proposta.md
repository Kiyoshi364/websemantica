# Proposta de Trabalho Final (DK Hashimoto)

O objetivo do trabalho é
construir uma interface de biblioteca
para descrever programas de [Prolog](https://en.wikipedia.org/wiki/Prolog)
que raciocínam sobre [Triplas RDF](https://en.wikipedia.org/wiki/Semantic_triple).

Uma outra forma de ver o trabalho é como
uma descrição de uma interface para
um banco de dados de triplas.
Isso inclui as quatro perguntas
que o [SPARQL](https://en.wikipedia.org/wiki/SPARQL) permite fazer:
1. Select
1. Ask
1. Construct
1. Describe

Eu dou ênfase em *interface*,
pois é importante que ela tenha três propriedades:
1. Boa representação semântica das triplas RDF
2. Integração natural com Prolog
3. Boa abstração de implementação

Cada uma dessas propriedades é importante
para uma pessoa (que vem de uma prespectiva) diferente.

A boa representação semântica das triplas RDF
é importante para quem é conhece o domínio
e pretende aplicar esse conhecimento
em uma linguagem de programação
(por exemplo, Python ou Prolog).

A integração natural com Prolog
é importante para quem já está acostumado com Prolog.
Essa pessoa não deveria reaprender a linguagem
simplesmente para usar uma biblioteca.

Já a boa abstração de implementação
permite que implementadores
troquem a implementação por de baixo dos panos
sem que a semantica do programa se altere;
podendo focar em fazer implementações mais eficientes.

## A implementação

Sendo definida a interface da biblioteca,
eu vou desenvolver biblioteca(s) que implementa(m) essa interface
(talvez mais de uma biblioteca)
na tentativa de mostrar
que as três propriedades desejadas
valem para essa interface.

Elas funcionariam como
um banco de triplas em memória.

Adicionalmente (se o tempo permitir),
posso implementar um parser de [Turtle (.ttl)](https://en.wikipedia.org/wiki/Turtle_(syntax))
em Prolog
para facilitar a entrada das triplas.

## Experimento

O experimento vai ser uma série de testes
para garantir
que as bibliotecas não fazem nada fora do esperado.

## O artigo

Título do artigo: A survey of RDF stores & SPARQL engines for querying knowledge graphs
Autor(es) do artigo: Waqas Ali, Muhammad Saleem, Bin Yao, Aidan Hogan, Axel-Cyrille Ngonga Ngomo
Nome do periódico/evento: The VLDB Journal (2022)
Classificação Qualis: A1
Ano de Publicação: 2022
Link: [https://link.springer.com/content/pdf/10.1007/s00778-021-00711-3.pdf](https://link.springer.com/content/pdf/10.1007/s00778-021-00711-3.pdf)

Esse artigo faz um resumão de várias estratégias
de implementação de bancos de triplas,
incluindo:
1. representação de armazenamento
1. indíces
1. joins
1. processamento de pesquisas
1. particionamento
Esse é o artigo base para o trabalho.

Título do artigo: ClioPatria: A SWI-Prolog Infrastructure for the Semantic Web
Autor(es) do artigo: Jan Wielemaker, Wouter Beek, Michiel Hildebrand, Jacco van Ossenbruggen
Ano de Publicação: 2015
Link: [https://www.semantic-web-journal.net/system/files/swj1074.pdf](https://www.semantic-web-journal.net/system/files/swj1074.pdf)

Esse artigo descreve uma suite de bibliotecas
que formam um framework
cujo objetivo principal é prototipação de programas.
Adicionalmente,
a biblioteca é implementada em C,
o que dificulta o resuso/adaptação da bibioteca
em outras implementações de Prolog.
