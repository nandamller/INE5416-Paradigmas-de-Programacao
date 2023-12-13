#!/bin/bash

# Caminho para o seu arquivo Prolog
PROLOG_FILE="reuniao_condominio.pl"

# Comando para executar o SWI-Prolog com o seu arquivo
# e a consulta que você quer fazer
swipl -s "$PROLOG_FILE" -g "solucao(ListaSolucao)."

# O comando acima faz o seguinte:
# - swipl: Inicia o SWI-Prolog
# - -s "$PROLOG_FILE": Carrega o arquivo Prolog especificado
# - -g "solucao(ListaSolucao),halt.": Executa a consulta e depois encerra o SWI-Prolog
