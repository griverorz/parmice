context("Random seeds")

test_that("Random seed is set in the clusters", {    

          ncores <- 2
          cl <- makeCluster(ncores)          
          registerDoParallel(cl)
          clusterSetRNGStream(cl, 31313)
          
          first <- parmice(nhanes, m=2, paropts=list(".combine"=ibind))
          clusterSetRNGStream(cl, 31313)
          second <- parmice(nhanes, m=2, paropts=list(".combine"=ibind))
          third <- parmice(nhanes, m=2, paropts=list(".combine"=ibind))
          clusterSetRNGStream(cl, 12437)
          fourth <- parmice(nhanes, m=2, paropts=list(".combine"=ibind))

          expect_equal(complete(first, action="long"), complete(second, action="long"))
          expect_false(isTRUE(all.equal(complete(second, action="long"),
                                        complete(third, action="long"))))
          expect_false(isTRUE(all.equal(complete(third, action="long"),
                                        complete(fourth, action="long"))))
          expect_false(isTRUE(all.equal(complete(second, action="long"),
                                        complete(fourth, action="long"))))
})


test_that("Imputations are different after setting the seed", {
    
          ncores <- 2
          cl <- makeCluster(ncores)
          registerDoParallel(cl)
          clusterSetRNGStream(cl, 31313)
          
          first <- parmice(nhanes, m=2, paropts=list(".combine"=ibind))

          expect_false(isTRUE(all.equal(complete(first, 1), complete(first, 2))))
          })
