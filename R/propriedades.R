calcula.propriedades <- function(dataset, rotulos){
  
  retorno = list()
  
  # convertendo de tipo FACTOR para NUMÉRICO
  labels.2 = data.frame(apply(rotulos, 2, as.numeric))
  
  # numero total de linhas = número total de instâncias
  retorno$num.instancias = nrow(flags)
  
  # número total de colunas = número total de atributos
  retorno$num.atributos = ncol(flags)
  
  # frequencia dos rótulos 
  # total de instâncias positivas e negativas por rótulo
  retorno$inst.neg.pos = data.frame(apply(labels.2, 2 , table))
  
  # coocorrencia de pares de rótulos
  freq.pares.rotulos = map_dfr(.x = combn(names(rotulos), 2, simplify = FALSE),
                               ~ rotulos %>%
                                 select(.x) %>%
                                 summarise(par_a = .x[1],
                                           par_b = .x[2],
                                           n = sum(rowSums(select(., everything()) == 1) == 2)))
  names(freq.pares.rotulos)[3] = "coocorrencia"
  retorno$freq.pares.rotulos = arrange(freq.pares.rotulos, desc(coocorrencia))
  
  # cardinalidade
  retorno$cardinalidade = mean(rowSums(labels.2))
  
  # densidade
  retorno$densidade = mean(rowSums(labels.2))/ncol(labels.2)
  
  # combinações de rótulos
  labelsets <- do.call(paste, c(rotulos, sep = ""))
  labelsets <- table(as.factor(labelsets))
  labelsets <- data.frame(labelsets)
  names(labelsets) = c("combinacao", "frequencia")
  retorno$labelsets = arrange(labelsets, frequencia)
  
  # total de combinações 
  retorno$total.combinacoes = nrow(labelsets)
  
  # combinações únicas
  labels.sets.unique = filter(labelsets, labelsets$frequencia==1)
  
  # total de comibinações únicas
  retorno$total.comb.unicas = nrow(labels.sets.unique)
  
  # combinações não únicas
  labels.sets = arrange(labelsets, frequencia)
  labels.sets = filter(labels.sets, labels.sets$frequencia!=1)
  retorno$total.comb.nao.unicas = nrow(labels.sets)
  
  # frequencia maxima
  retorno$labelset.freq.max = max(labels.sets$frequencia)
  
  # frequencia minima
  retorno$labelset.freq.min =  min(labels.sets$frequencia)
  
  # total de instâncias atribuídas a X rótulos
  unique = labels.2 %>% mutate(total = rowSums(labels.2))
  unique = arrange(unique, total)
  frequencia = data.frame(count(unique, total))
  names(frequencia) = c("total", "frequencia")
  
  # total de instâncias atribuidas a um único rótulo
  inst.rotulo.unico = filter(frequencia, frequencia==1)
  retorno$inst.rotulo.unico = inst.rotulo.unico$frequencia
  
  #  
  retorno$freq.rot.inst = frequencia
  
  # DESBALANCEAMENTO
  # conta as colunas
  label.count = colSums(labels.2)
  
  # frequencia
  label.freq = label.count / nrow(labels.2)
  
  # desbalanceamento
  IRLbl = max(label.count) / label.count
  retorno$IRLbl = IRLbl
  
  # média do desbalanceamento
  retorno$meanIR = mean(IRLbl)
  
  # maximo IR
  retorno$maxIR = max(IRLbl)
  
  # tcs
  retorno$tcs = log(abs(ncol(flags) * ncol(labels.2) * nrow(labelsets)))
  
  return(retorno)
  gc()
  
}