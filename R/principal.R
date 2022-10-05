# diretórios
FolderRoot = "~/Propriedades-Multirrotulo"
FolderScripts = "~/Propriedades-Multirrotulo/R"
FolderDados = "~/Propriedades-Multirrotulo/Dados"
FolderResultados = "~/Propriedades-Multirrotulo/Resultados"

# carregando outros scripts
setwd(FolderScripts)
source("bibliotecas.R")

setwd(FolderScripts)
source("propriedades.R")

# abrindo o dataset
nome.arff = paste(FolderDados, "/flags.arff", sep="")
flags = data.frame(read.arff(nome.arff))

# separando o espaço de rótulos
labels = data.frame(flags[,c(20:26)])

# chama a função
res = calcula.propriedades(flags, labels)
res


nomes.arquivos = dir(FolderDados)
tamanho = length(nomes.arquivos)/2

setwd(FolderRoot)
datasets = data.frame(read.csv("datasets-info.csv"))

# emotions = 26
# flags = 34
# scene = 62

indices = c(26,34,62)
todos = list()

i = 1
while(i<=tamanho){
  
  # obtendo informação do dataset
  ds = datasets[indices[i],]
  
  cat("\n", ds$Name)

  # criando o nome do arquivo
  nome.arff = paste(FolderDados, "/", ds$Name, ".arff", sep="")
  
  # abrindo o dataset
  multi.label.data = data.frame(read.arff(nome.arff))
  
  # separando o espaço de rótulos
  labels = data.frame(multi.label.data[,ds$LabelStart:ds$LabelEnd])
  
  # chama a função
  res = calcula.propriedades(multi.label.data, labels)
  print(res)
  
  i = i + 1
  gc()
}
