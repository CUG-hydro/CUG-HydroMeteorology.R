# install.packages("remotes")
# remotes::install_github("rpkgs/hydroTools")
library(hydroTools)
library(data.table)
# library(lubridate)

df = fread("data-raw/processed_57494_武汉.csv")
d = df[year(date) == 2019, ]
q = RH2q(d$RH_avg, d$Pa_avg/100, d$Tair_avg/10)
plot(q)
fwrite(data.table(q), "q.csv")
