library(ess)

turkey <-
  ess_country(
    country = "Turkey",
    rounds = c(2, 4),
    your_email = "fh@ifs.ku.dk"
  )

tr2<-turkey[[1]]
