fileConn<-file("ED2_vars.txt")
sink('ED2_vars.txt')

writeLines(print(mydata1), fileConn)

print(head(mydata1, 250))

grep(pattern = "NPP", x = names(mydata1), value = T)

close(fileConn)

options(max.print=999999)



NPPDaily <- mydata1[["MMEAN_NPPDAILY"]][]

NPPcroot <- mydata1[["MMEAN_NPPCROOT"]][]
NPPfroot <- mydata1[["MMEAN_NPPFROOT"]][]
NPPleaf <- mydata1[["MMEAN_NPPLEAF"]][]
NPPsapwood <- mydata1[["MMEAN_NPPSAPWOOD"]][]
NPPseeds <- mydata1[["MMEAN_NPPSEEDS"]][]
NPPwood <- mydata1[["MMEAN_NPPWOOD"]][]

sum(NPPcroot, NPPfroot, NPPleaf, NPPsapwood,NPPseeds, NPPwood)

AGE            = mydata1[["SLZ"]][]

