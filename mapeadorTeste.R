
nome.arquivo <- "MNaprava.png"
leitura.arquivo <- file(nome.arquivo, "rb")
arquivo.raw <- rawToBits(readBin(leitura.arquivo, raw(), file.info(nome.arquivo)$size))

arquivo.tamanho <- file.info(nome.arquivo)$size

pb <- txtProgressBar(min = 0,
                     max = arquivo.tamanho,
                     style = 3,
                     width = 50,
                     char = "=")

mapaBits <- function(vetorRawBinario)
{
  png(file = "mapabitsmnaprava.png", bg = "transparent")
  plot(0, 0, xlim = c(0, 8), ylim = c(0, arquivo.tamanho), col = "white", axes = FALSE)
  x <- sapply(0:(arquivo.tamanho*8 - 1), function(i) {
    polygon(c(0:1, 1:0) + i %% 8, c(0, 0:1, 1) + i %/% 8,
            col = c("blue", "grey")[as.integer(vetorRawBinario[i + 1]) + 1],
            border = NA)
    setTxtProgressBar(pb, i)
  })
  dev.off()
}

mapaBits(arquivo.raw)