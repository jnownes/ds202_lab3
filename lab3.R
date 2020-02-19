dat <- readxl::read_xls('GSS.xls')
head(dat)
ggplot(dat, aes(x=`Marital status`, y = `General happiness`)) + geom_point()
