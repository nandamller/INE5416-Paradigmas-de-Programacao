# Link para os tabuleiros usados: https://www.janko.at/Raetsel/Kojun/index.htm
# Tabuleiros usados: 3, 6, 15
class Tabuleiro:

    @staticmethod
    def get_valores_tabuleiro6x6():
        val_tabuleiro = [[0, 0, 0, 0, 0, 2],
                         [2, 0, 0, 5, 0, 0],
                         [0, 0, 3, 0, 0, 4],
                         [0, 0, 0, 3, 0, 1],
                         [0, 0, 0, 0, 0, 0],
                         [0, 0, 3, 0, 2, 5]]

        return val_tabuleiro

    @staticmethod
    def get_regioes_tabuleiro6x6():
        reg_tabuleiro = [[0, 1, 2, 2, 3, 3],
                         [0, 1, 4, 3, 3, 3],
                         [0, 0, 4, 4, 4, 5],
                         [6, 6, 7, 5, 5, 5],
                         [6, 6, 7, 8, 9, 9],
                         [10, 10, 8, 8, 8, 8]]

        return reg_tabuleiro

    @staticmethod
    def get_valores_tabuleiro8x8():
        val_tabuleiro = [[2, 0, 7, 0, 3, 0, 0, 0],
                         [0, 0, 0, 0, 0, 0, 1, 0],
                         [0, 4, 0, 4, 7, 6, 0, 0],
                         [0, 0, 0, 0, 1, 4, 1, 3],
                         [0, 0, 0, 6, 4, 3, 0, 0],
                         [0, 3, 0, 0, 0, 0, 2, 0],
                         [0, 0, 3, 2, 6, 0, 0, 3],
                         [1, 4, 0, 0, 0, 0, 3, 0]]

        return val_tabuleiro

    @staticmethod
    def get_regioes_tabuleiro8x8():
        reg_tabuleiro = [[0, 0, 0, 1, 1, 1, 2, 2],
                         [0, 0, 3, 3, 4, 4, 2, 5],
                         [6, 0, 7, 3, 8, 8, 8, 5],
                         [6, 0, 7, 3, 3, 8, 8, 8],
                         [9, 9, 10, 11, 11, 11, 11, 8],
                         [9, 9, 10, 13, 11, 11, 12, 12],
                         [14, 15, 13, 13, 13, 13, 16, 12],
                         [14, 14, 14, 13, 16, 16, 16, 12]]

        return  reg_tabuleiro

    @staticmethod
    def get_valores_tabuleiro10x10():
        val_tabuleiro = [[0, 4, 3, 0, 2, 5, 0, 0, 0, 0],
                         [0, 2, 0, 0, 0, 4, 2, 0, 3, 0],
                         [0, 0, 0, 1, 4, 0, 0, 1, 0, 0],
                         [5, 6, 0, 2, 3, 0, 5, 0, 0, 0],
                         [0, 3, 5, 0, 0, 0, 3, 0, 0, 0],
                         [0, 0, 0, 7, 0, 7, 0, 5, 0, 4],
                         [0, 0, 5, 3, 0, 2, 0, 4, 0, 0],
                         [0, 0, 1, 5, 0, 0, 0, 5, 3, 0],
                         [1, 3, 7, 0, 0, 0, 6, 0, 0, 5],
                         [2, 1, 0, 0, 3, 0, 1, 0, 3, 4]]

        return val_tabuleiro

    @staticmethod
    def get_regioes_tabuleiro10x10():
        reg_tabuleiro = [[0, 1, 1, 1, 1, 1, 2, 3, 4, 4],
                         [0, 0, 1, 1, 2, 2, 2, 3, 3, 5],
                         [0, 0, 6, 6, 6, 7, 8, 3, 3, 5],
                         [6, 6, 6, 9, 9, 7, 8, 10, 11, 12],
                         [6, 13, 13, 9, 14, 7, 8, 10, 11, 12],
                         [13, 13, 14, 14, 14, 15, 8, 11, 11, 11],
                         [13, 13, 14, 14, 15, 15, 8, 8, 8, 16],
                         [17, 17, 14, 18, 15, 15, 15, 15, 16, 16],
                         [17, 17, 18, 18, 18, 18, 18, 16, 16, 16],
                         [19, 19, 19, 19, 18, 20, 20, 20, 20, 16]]

        return reg_tabuleiro

    def get_tabuleiro_tamanho(self, tamanho):
        if tamanho == 6:
            valores = self.get_valores_tabuleiro6x6()
            regioes = self.get_regioes_tabuleiro6x6()
            return valores, regioes

        if tamanho == 8:
            valores = self.get_valores_tabuleiro8x8()
            regioes = self.get_regioes_tabuleiro8x8()
            return valores, regioes

        if tamanho == 10:
            valores = self.get_valores_tabuleiro10x10()
            regioes = self.get_regioes_tabuleiro10x10()
            return valores, regioes

        return False, False
