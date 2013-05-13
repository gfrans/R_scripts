##############
# Useful libs:
##############

library(signal)
library(mFilter)
library(tuneR)
library(seewave)




##########################
# Useful environment vars:
##########################

e <- new.env()
e$AFC1 <- "A/F Correction 1 (%)"
e$AFL1 <- "A/F Learning 1 (%)"
e$MAFV <- "MAF Volt. (V)"
e$LOGDIR <- "C:\\Users\\greg\\Documents\\APLogs\\"
e$KNOCKDIR <- "C:\\Users\\greg\\Videos\\GoPro\\Saab\\Knock\\"
e$CSVEXT <- "*.csv"
e$WAVEXT <- "*.wav"




#######################
# Wave file operations:
#######################

#########################
# simple low pass filter:
#y - vector to filter
#t - time interval between measurements (s)
#f - low pass frequency (Hz)
lpf <- function( y, t, f ) {
  rc <- 1 / ( 2 * pi * f )
  a  <- t / ( t + rc )
  n  <- length( y )
  yf <- y
  for( i in 2:length(y) ) {
    yf[i] <- a * y[i] + (1-a) * yf[i-1]
  }
  return( yf )
}  

##########################
# Simple high pass filter:
#y - given input samples
#t - time interval dt (s)
#f - time constant RC (Hz)
hpf <- function( y, t, f ) {
  rc <- 1 / ( 2 * pi * f )
  a <- rc / ( rc + t ) 
  n <- length( y )
  yf <- y
  for( i in 2:n ) {
    yf[i] <- a * ( yf[i - 1] + y[i] - y[i - 1])
  }
  return( yf )
}

###############################################
# Filter all WAV files in specified directory:
# pbh - high cutoff for passband
# phl - low cutoff for passband
passbandAllWavs <- function(pbh = 10000, pbl = 10) {
  wavs <- choose.files(default = paste0(e$KNOCKDIR,e$WAVEXT), caption = "Select WAV files")
  if ( length(wavs) ) {
    for (i in 1:length(wavs)){
      print(wavs[i])
      wav <- readWave(wavs[i])

      # run low pass filter with high cutoff
      lchannel <- lpf(wav@left, (1 / wav@samp.rate), pbh)
      rchannel <- lpf(wav@right, (1 / wav@samp.rate), pbh)

      # run high pass filter with low cutoff
      lchannel <- hpf(lchannel, (1 / wav@samp.rate), pbl)
      rchannel <- hpf(rchannel, (1 / wav@samp.rate), pbl)

      # recombine channels and generate new WAV file
      newwav <- Wave(left = as.integer(lchannel), right = as.integer(rchannel), samp.rate = wav@samp.rate, bit = wav@bit)

      writeWave(newwav, paste0(dirname(wavs[1]), "\\", i, "_h", pbh, "_l", pbl, ".wav"))
    }
  }
  else
    return(NULL)
}




##########################
# CSV log functions:
##########################

##########################################
# Plot all CSV log files in specified dir:
plotAllLogs <- function() {
  logs <- choose.files(default = paste0(e$LOGDIR,e$CSVEXT), caption = "Select log files")
  if ( length(logs) ) {
    pdf(paste0(dirname(logs[1]), "_plots.pdf"), width = 11, height = 8)
    close.screen(all = TRUE)
    numwins <- ceiling(sqrt(length(logs)))
    split.screen(figs = c(numwins, numwins))
    for (i in 1:length(logs)){
      f <- file(logs[i])
      screen(i)
      plotAFRCvsMAFV(parseCSV(f), fname = basename(logs[i]))
      close(f)
    }
    close.screen(all = TRUE)
    par(mar = c(0,0,.8,0))
    title(main = dirname(logs[1]))
    dev.off()
  }
  else
    return(NULL)
}
 
###################################
# Parse CSV log file into a matrix:
#f - a file handle to the CSV
parseCSV <- function( f ) {
  contents <- readLines(f, -1)
  columns <- strsplit(contents[1], ",")[[1]]
  clen <- length(columns)
  lc <- length(contents)
  csvmatrix <- matrix((lc - 1), clen, data = 0, dimnames = list(NULL, columns))
  for(i in 2:lc){
    columns <- strsplit(contents[i], ",")[[1]]
    for (j in 1:clen){
      csvmatrix[i - 1,j] <- columns[j]
    } 
  }
  return(csvmatrix)
}

#############################
# Generate AFRC vs MAFV plot:
#m - matrix derived from CSV file
#fname - title to use for plot 
plotAFRCvsMAFV <- function( m, fname = "none" ) {
  columns = dimnames(m)[[2]]
  indices <- NULL
  if (e$AFC1 %in% columns)
    indices <- c(indices, match(e$AFC1, columns))
 
  if (e$AFL1 %in% columns) 
    indices <- c(indices, match(e$AFL1, columns))

  if (e$MAFV %in% columns){
    mvcol <- match(e$MAFV, columns)
  }
  else
    return(NULL)

  if (length(indices) == 0)
    return(NULL)

  AFRC <- NULL

  for (i in 1:dim(m)[1]) {
    AFRC <- c(AFRC, Reduce('+', as.numeric(m[i, indices])))
  }

  plot(x = m[, mvcol], y = AFRC, main = "AFR Corrections vs. MAF Voltage", xlab = "MAF Voltage",
       ylab = "AFR Correction", sub = fname, pch = 20, cex = .2)
  abline(lm(AFRC~as.numeric(m[, mvcol])))

}
