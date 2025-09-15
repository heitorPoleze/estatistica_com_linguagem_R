vetLetras = c("C","L","L","C","S","La","C","C","L","M","C","M","So","M", "L", "C", "C","M","C", "L")

tabela.vet = table(vet)

df = matrix(0, 5, 3)

colnames(df) = c("FA", "FR", "FP")

rownames(df) = c("Cafe","Leite", "Milho", "Outros", "Total")

df[1,1] = tabela.vet["C"]
df[2,1] = tabela.vet["L"]
df[3,1] = tabela.vet["M"]
df[4,1] = sum(tabela.vet["La"],tabela.vet["So"], tabela.vet["S"])
df[5,1] = sum(df[-5,1])

for (i in 1:5){
  df[i,2] = df[i, 1]/df[5,1]
  df[i,3] = df[i,2] * 100
}

matrizQtdFilhos = matrix(0,6,3)
teste = matrix(1,6,3)
colnames(matrizQtdFilhos) = c("FA", "FR", "%")
rownames(matrizQtdFilhos) = c("0","1","2","3","4", "Total")


matrizQtdFilhos[1,1] = 3
matrizQtdFilhos[2,1] = 10
matrizQtdFilhos[3,1] = 9
matrizQtdFilhos[4,1] = 11
matrizQtdFilhos[5,1] = 7
matrizQtdFilhos[6,1] = sum(matrizQtdFilhos[-6,1])

for(i in 1:6){
  matrizQtdFilhos[i,2] = matrizQtdFilhos[i,1]/matrizQtdFilhos[6,1]
  matrizQtdFilhos[i,3] = matrizQtdFilhos[i,2] * 100
}

graficoQtdFilhos = pie(matrizQtdFilhos[1:5,1], col = rainbow(5), radius=1)
