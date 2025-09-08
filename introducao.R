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

grafico1 = barplot(df[1:4,3], 
                   ylab = "Frequência Porcentagem", 
                   xlab = "Atividades",
                   col = gray(seq(0.4,1.0, length = 4)))
