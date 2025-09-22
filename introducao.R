pesosEmKg <- c(19.1, 30.6, 34.2, 37.2, 39.4, 44.3, 50.1, 
           20.0, 31.0, 35.2, 38.6, 39.4, 44.4, 56.2, 
           23.3, 31.1, 35.3, 38.7, 41.0, 44.5, 57.1, 
           24.2, 32.0, 36.3, 39.0, 42.1, 45.2, 57.2, 
           28.2, 32.0, 36.5, 39.0, 42.2, 47.5, 58.2, 
           28.2, 33.1, 36.7, 39.1, 43.0, 48.3, 60.2, 
           30.5, 34.0, 37.2, 39.2, 43.0, 49.2, 60.3)

df = matrix(0,8,3)

colnames(df) = c('FA', 'FR', 'FP')
rownames(df) <- c("[19,1;30,6)", "[30,6;34,2)", "[34,2;37,2)","[37,2;39,4)", "[39,4;44,3)", "[44,3;50,1)", "[50,1;60,3]", "Total")

tab.pesosEmKg = table(cut(pesosEmKg, breaks = c(19.1, 30.6, 34.2, 37.2, 39.4, 44.3, 50.1, 60.3)))

df[1:7,1] = tab.pesosEmKg
df[8,1] = length(pesosEmKg)

for(i in 1:8){
  df[i,2] = round(df[i,1]/length(pesosEmKg), digits = 2)
  df[i,3] = df[i,2]*100
}


h <- hist(pesosEmKg, breaks=c(19.1, 30.6, 34.2, 37.2, 39.4, 44.3, 50.1, 60.3),
          freq=FALSE, ylab="Dfr", xlab="Peso das Crianças (Kg)", main="",
          col=topo.colors(7))
points(h$mids, h$density, "l", lwd=2)

