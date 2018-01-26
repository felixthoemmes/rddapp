context("McCrary Sorting Test")

if (.Platform$OS.type == "windows") {
  path <- "C:/Academia/Cornell/QML/RDD/rddapp/proj/reference/dc_test"
} else {
  path <- "~/rddapp/proj/reference/dc_test"
}

# dc test
if (file.exists(path)) {
  setwd(path)
  
  # data
  # read in data from ftp://k7moa.com/hdemrep35_113.dat
  local_test <- file.exists("hdemrep35_113.dat")
  
  if (local_test) {
    rollcall <- read.table("hdemrep35_113.dat")
    names(rollcall) <- c("Congress number",
                         "Roll Call number",
                         "Month",
                         "Day",
                         "Year",
                         "Number Missing Votes (not voting and not in Congress)",
                         "Number Yeas",
                         "Number Nays",
                         "Number Republican Yeas",
                         "Number Republican Nays",
                         "Number Democrat Yeas",
                         "Number Democrat Nays",
                         "Number Northern Republican Yeas",
                         "Number Northern Republican Nays",
                         "Number Southern Republican Yeas (11 States of Confederacy plus KY and OK)",
                         "Number Southern Republican Nays",
                         "Number Northern Democrat Yeas",
                         "Number Northern Democrat Nays",
                         "Number Southern Democrat Yeas (11 States of Confederacy plus KY and OK)",
                         "Number Southern Democrat Nays")
    
    # create percentage yay
    rollcall$peryea <- rollcall$'Number Yeas' / (rollcall$'Number Yeas' + rollcall$'Number Nays')
    
    # trim data so that it includes the exact same number of rows are reported in McCrary
    rollcall2004 <- rollcall[1:35052, ]
    
    # test
    
    test_that("one assignment var", {
      # ground truth from McCrary
      # t-stat 6.6
      # point estimate: .521
      # se: .079
      
      rddapp_dc <- rddapp::dc_test((rollcall2004$peryea), cutpoint = .5, bin = .003, bw = .03, 
                                   plot = FALSE, ext.out = TRUE)
      rdd_dc <- rdd::DCdensity((rollcall2004$peryea), cutpoint = .5, bin = .003, bw = .03, 
                               plot = FALSE, ext.out = TRUE)
      
      # theta
      expect_equal(rddapp_dc$theta, rdd_dc$theta)
      expect_equal(rdd_dc$theta, .521, tolerance = 5e-2)
      
      # se
      expect_equal(rddapp_dc$se, rdd_dc$se)
      expect_equal(rdd_dc$se, .079, tolerance = 5e-3)
      
      # z
      expect_equal(rddapp_dc$z, rdd_dc$z)
      expect_equal(rdd_dc$z, 6.6, tolerance = 5e-2)
    })
  }
}  
  




