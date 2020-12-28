#workflow for image acquisition

#read in png
im <- imager::load.image("~/Documents/GitHub/specimR/test/Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39.png")
#function to equalise intensity values
hist.eq <- function(im) imager::as.cimg(ecdf(im)(im),dim=dim(im))
#split image into rgb channels
cn <- imager::imsplit(im,"c")
#equalise each channel individually
cn.eq <- imager::map_il(cn,hist.eq)
#recombine and plot
im2 <- imager::imappend(cn.eq,"c") %>% plot(main="All channels equalised")

