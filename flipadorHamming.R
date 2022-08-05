options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

library("tools")

nome.arquivo <- args[1]

leitura.arquivo <- file(nome.arquivo, "rb")
arquivo.tamanho.bits <- 8*(file.info(nome.arquivo)$size)

nome.arquivo.semwham <- gsub(".wham", "", nome.arquivo)
extensao.arquivo <- file_ext(nome.arquivo.semwham)
nome.arquivo.final <- paste0(gsub(extensao.arquivo, "", nome.arquivo.semwham), "Flipado", ".", extensao.arquivo, ".wham")

escrita.arquivo <- file(nome.arquivo.final, "wb")

arquivo.binario <- rawToBits(readBin(leitura.arquivo, raw(), arquivo.tamanho.bits))

num.flips <- args[2]
posicoes.flipadas <- sample(1:(arquivo.tamanho.bits), num.flips, replace = FALSE)

arquivo.binario[posicoes.flipadas] <- sapply(arquivo.binario[posicoes.flipadas], function(x) xor(x, as.raw(1)))

writeBin(packBits(arquivo.binario, "raw"), escrita.arquivo)

close(leitura.arquivo)
close(escrita.arquivo)