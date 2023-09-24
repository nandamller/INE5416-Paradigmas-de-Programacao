from kojun import Kojun
from tabuleiros import Tabuleiro

if __name__ == '__main__':

    t = Tabuleiro()

    while 1:
        print("Opções de tamanho: 6, 8, 10 \nDigite o tamanho do tabuleiro para resolver: ", end="")
        tamanho = int(input())

        valores, regioes = t.get_tabuleiro_tamanho(tamanho)

        if valores:
            k = Kojun(valores, regioes, tamanho)
            k.resolver_tabuleiro(0, 0)
            k.print_tabuleiro_final()
            break

        print("\nNão existe tabuleiro com esse tamanho no banco\n\n")
