options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

gerador.hamming <- function (byte) {
  
  c1 <- Reduce(`xor`, byte[c(1, 2, 4, 5, 7)])
  c2 <- Reduce(`xor`, byte[c(1, 3, 4, 6, 7)])
  c4 <- Reduce(`xor`, byte[c(2:4, 8)])
  c8 <- Reduce(`xor`, byte[c(5:8)])

  novovetor <- raw(16)
  
  novovetor[2] <- c1
  novovetor[3] <- c2
  novovetor[4] <- byte[1]
  novovetor[5] <- c4
  novovetor[6] <- byte[2]
  novovetor[7] <- byte[3]
  novovetor[8] <- byte[4]
  novovetor[9] <- c8
  novovetor[10] <- byte[5]
  novovetor[11] <- byte[6]
  novovetor[12] <- byte[7]
  novovetor[13] <- byte[8]
  novovetor[14] <- Reduce(`xor`, novovetor[c(2:13)])
  
  return(novovetor)
}

nome.arquivo <- args

leitura.arquivo <- file(nome.arquivo, "rb")

arquivo.tamanho <- file.info(nome.arquivo)$size

escrita.arquivo <- file(paste0(nome.arquivo, ".wham"), "wb")

pb <- txtProgressBar(min = 0,
                     max = arquivo.tamanho,
                     style = 3,
                     width = 50,
                     char = "=")

for (byte.arq.bin in 1:arquivo.tamanho){
  arquivo.byte <- rawToBits(readBin(leitura.arquivo, raw(), 1))
  byte.hamming <- packBits(gerador.hamming(arquivo.byte), "raw")
  writeBin(byte.hamming, escrita.arquivo)
  setTxtProgressBar(pb, byte.arq.bin)
}

close(pb)
close(leitura.arquivo)
close(escrita.arquivo)
