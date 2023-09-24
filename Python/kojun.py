class Kojun:
    def __init__(self, valores, regioes, tamanho):
        self.val_tabuleiro = valores
        self.reg_tabuleiro = regioes
        self.tamanho = tamanho
        self.qtde_regioes = max(max(row) for row in self.reg_tabuleiro) + 1
        self.regioes_mapeadas = self.mapear_regioes()

    def mapear_regioes(self):
        map_regioes = [[] for regiao in range(self.qtde_regioes)]

        for linha in range(self.tamanho):
            for coluna in range(self.tamanho):
                id_regiao = self.reg_tabuleiro[linha][coluna]
                map_regioes[id_regiao].append((linha, coluna))

        return map_regioes

    def print_tabuleiro_final(self):
        for linha in self.val_tabuleiro:
            for coluna in linha:
                print(coluna, end=" ")
            print()

    def resolver_tabuleiro(self, i, j):
        if i == self.tamanho - 1 and j == self.tamanho:
            return True, self.val_tabuleiro
        elif j == self.tamanho:
            return self.resolver_tabuleiro(i + 1, 0)
        elif self.val_tabuleiro[i][j] > 0:
            return self.resolver_tabuleiro(i, j + 1)
        else:
            max_num = len(self.regioes_mapeadas[self.reg_tabuleiro[i][j]])
            return self.avaliar_numeros(1, max_num, i, j)

    def avaliar_numeros(self, num, max_num, i, j):
        if num > max_num:
            return False, self.val_tabuleiro

        else:
            if self.numero_possivel(i, j, num):
                self.val_tabuleiro[i][j] = num
                (resultado, matriz) = self.resolver_tabuleiro(i, j + 1)
                if resultado:
                    return resultado, matriz
                else:
                    self.val_tabuleiro[i][j] = 0
                    return self.avaliar_numeros(num + 1, max_num, i, j)
            else:
                return self.avaliar_numeros(num + 1, max_num, i, j)

    def numero_possivel(self, linha, coluna, num):
        id_regiao = self.reg_tabuleiro[linha][coluna]
        regiao = self.regioes_mapeadas[id_regiao]

        # Como isso funciona?
        for (im, jm) in regiao:
            if self.val_tabuleiro[im][jm] == num:
                return False

        if (linha - 1 >= 0) and (self.val_tabuleiro[linha - 1][coluna] == num):
            return False
        if (linha + 1 < self.tamanho) and (self.val_tabuleiro[linha + 1][coluna] == num):
            return False
        if (coluna - 1 >= 0) and (self.val_tabuleiro[linha][coluna - 1] == num):
            return False
        if (coluna + 1 < self.tamanho) and (self.val_tabuleiro[linha][coluna + 1] == num):
            return False

        for it in range(linha - 1, -1, -1):
            if self.reg_tabuleiro[it][coluna] != self.reg_tabuleiro[linha][coluna]:
                break
            if self.val_tabuleiro[it][coluna] < num:
                return False

        for it in range(linha + 1, self.tamanho):
            if self.reg_tabuleiro[it][coluna] != self.reg_tabuleiro[linha][coluna]:
                break
            if self.val_tabuleiro[it][coluna] > num:
                return False

        return True
