options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

library("tools")

verificador.hamming <- function (byte.hamming) {
  
  t1 <- Reduce(`xor`, byte.hamming[c(2, 4, 6, 8, 10, 12)])
  t2 <- Reduce(`xor`, byte.hamming[c(3, 4, 7, 8, 11, 12)])
  t4 <- Reduce(`xor`, byte.hamming[c(5, 6, 7, 8, 13)])
  t8 <- Reduce(`xor`, byte.hamming[c(9, 10:13)])
  tg <- as.integer(Reduce(`xor`, byte.hamming[c(2:14)]))
  
  if(tg != 0) {
    palavra.sindrome <- c(t1, t2, t4, t8)
    bit.flipado <- packBits(c(palavra.sindrome, as.raw(rep(0, 28))), "integer") + 1
    byte.hamming[bit.flipado] <- xor(byte.hamming[bit.flipado], as.raw(1))
  }

  byte <- raw(8)
  
  byte[1] <- byte.hamming[4]
  byte[2] <- byte.hamming[6]
  byte[3] <- byte.hamming[7]
  byte[4] <- byte.hamming[8]
  byte[5] <- byte.hamming[10]
  byte[6] <- byte.hamming[11]
  byte[7] <- byte.hamming[12]
  byte[8] <- byte.hamming[13]
  
  retorno.resultado <- list("estado" = tg, "bytecorr" = byte)
  
  return(retorno.resultado)
}


nome.arquivo <- args
leitura.arquivo <- file(nome.arquivo, "rb")

nome.arquivo.semwham <- gsub(".wham", "", nome.arquivo)
extensao.arquivo <- file_ext(nome.arquivo.semwham)
nome.arquivo.final <- paste0(gsub(extensao.arquivo, "", nome.arquivo.semwham), "Corrigido", ".", extensao.arquivo)

arquivo.tamanho <- file.info(nome.arquivo)$size
arquivo.tamanho.duplo <- arquivo.tamanho/2

escrita.arquivo <- file(nome.arquivo.final, "wb")

pb <- txtProgressBar(min = 0,
                     max = arquivo.tamanho.duplo,
                     style = 3,
                     width = 50,
                     char = "=")
estadoByte <- NULL

for (byte.arq.bin in 1:arquivo.tamanho.duplo){
  arquivo.byte <- rawToBits(readBin(leitura.arquivo, raw(), 2))
  retorno.verificador <- verificador.hamming(arquivo.byte)
  estadoByte <- append(estadoByte, retorno.verificador$estado)
  byte <- packBits(retorno.verificador$bytecorr, "raw")
  writeBin(byte, escrita.arquivo)
  setTxtProgressBar(pb, byte.arq.bin)
}

close(pb)
close(leitura.arquivo)
close(escrita.arquivo)

mapaBytes <- function(vetorEstados)
{
  pb_Mapa <- txtProgressBar(min = 0,
                       max = arquivo.tamanho,
                       style = 3,
                       width = 50,
                       char = "=")
  nome.arquivo.mapa <- paste0(nome.arquivo.semwham, ".Visualizacao.png")
  png(file = nome.arquivo.mapa, width = 4000, height = 8000, bg = "transparent")
  plot(0, 0, xlim = c(0, 8), ylim = c(0, arquivo.tamanho), col = "white", axes = FALSE)
  x <- sapply(0:(arquivo.tamanho*8 - 1), function(i) {
    polygon(c(0:1, 1:0) + i %% 8, c(0, 0:1, 1) + i %/% 8,
            col = c("green", "red", "grey")[vetorEstados[i + 1] + 1],
            border = NA)
  setTxtProgressBar(pb_Mapa, i)
  })
}

mapaBytes(estadoByte)

