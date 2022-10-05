FolderRoot = "~/Propriedades-Multirrotulo"
FolderScripts = "~/Propriedades-Multirrotulo/R"
FolderDados = "~/Propriedades-Multirrotulo/Dados"

setwd(FolderScripts)
source("bibliotecas.R")

# abrindo o dataset
nome.arff = paste(FolderDados, "/flags.arff", sep="")
flags = data.frame(read.arff(nome.arff))

#head(flags)
# View(flags)
# summary(flags)
# apply(flags, 2, sd)

# numero total de linhas = número total de instâncias
nrow(flags)

# número total de colunas = número total de atributos
ncol(flags)

# separando o espaço de rótulos
labels = data.frame(flags[,c(20:26)])

#head(labels)
#View(labels)
summary(labels)

# convertendo de tipo FACTOR para NUMÉRICO
labels.2 = data.frame(apply(labels, 2, as.numeric))

# class(labels)
# class(labels$red)
summary(labels.2)

# somando os rótulos
soma = data.frame(apply(labels.2, 2, sum))

# obtendo a frequencia de 0s e 1s
frequency = data.frame(apply(labels.2, 2 , table))

#labels$red==1

#count(labels, (labels$red==1 & labels$orange==1), name = "frequency")


setwd(FolderDados)
write.csv(labels, "rotulos-flags.csv", row.names = FALSE)

count(labels, (labels[,1]==1 & labels[,2]==1), name = "frequency")

resultado = map_dfr(.x = combn(names(labels), 2, simplify = FALSE),
                    ~ labels  %>%
                      select(.x) %>%
                      summarise(par_a = .x[1],
                                par_b = .x[2],
                                n = sum(rowSums(select(., everything()) == 1) == 2)))

names(resultado)[3] = "coocorrencia"

arrange(resultado,desc(coocorrencia))

x <- "10"
x <- as.numeric(x)
x %<>% as.numeric 

x <- sapply(labels, is.factor)
labels[ , x] <- as.data.frame(apply(labels[ , x], 2, as.numeric))
class(labels$red)

cardinality <- function(labels){
  add = 0  
  r = nrow(labels)
  c = ncol(labels)
  
  for(i in 1:r){
    for(j in 1:c){
      add = add + sum(labels[i,j])
      #cat("\ni = ", i, " | j =", j, " | soma = ", add)
    }
    gc()
  }
  card = (1/r) * (add)
  return(card)
  gc()
}

density <- function(labels){
  add = 0
  r = nrow(labels)
  c = ncol(labels)  
  for(i in 1:r){
    for(j in 1:c){
      add = add + (sum(labels[i,j])/c)
    }
    gc()
  }
  den = (1/r) * (add)
  return(den)
  gc()
}

cardinality(labels)
time.card = system.time(cardinality(labels.2))

density(labels)
time.dens = system.time(density(labels))

card = mean(rowSums(labels.2))
time.card.2 = system.time(mean(rowSums(labels)))

dens = mean(rowSums(labels.2))/ncol(labels.2)
time.dens.2 = system.time(mean(rowSums(labels))/ncol(labels))


labelsets <- do.call(paste, c(labels, sep = ""))
labelsets <- table(as.factor(labelsets))
labelsets <- data.frame(labelsets)
names(labelsets) = c("combinacao", "frequencia")
labelsets = arrange(labelsets, frequencia)

labels.sets.unique = filter(labelsets, labelsets$frequencia==1)
num.total.comb = nrow(labelsets)
num.total.comb.unique = nrow(labels.sets.unique)

labels.sets = arrange(labelsets, frequencia)
labels.sets = filter(labels.sets, labels.sets$frequencia!=1)
label.set.freq.max = max(labels.sets$frequencia)
label.set.freq.min =  min(labels.sets$frequencia)


unique = labels.2 %>% mutate(total = rowSums(labels.2))
unique = arrange(unique, total)
frequencia = data.frame(count(unique, total))
names(frequencia) = c("total", "frequencia")

# conta as colunas
label.count = colSums(labels.2)

# frequencia
label.freq = label.count / nrow(labels.2)

# desbalanceamento
IRLbl = max(label.count) / label.count

# média do desbalanceamento
meanIR = mean(IRLbl)

# maximo
maxIR = max(IRLbl)

num.att = ncol(flags)
num.instances = nrow(flags)
num.labels = ncol(labels)
num.label.sets = length(labelsets)
tcs = log(abs(num.att - 2 - num.instances * num.instances * num.label.sets))
tcs = log(abs(num.att * num.instances * num.labels * num.label.sets))
tcs = log(abs(num.att * num.labels * num.label.sets))






