#!/bin/bash

swipl -s kojun.pl -g "set_prolog_flag(answer_write_options, [max_depth(0)]), solucao(Result), write(Result), nl, halt." -t halt
