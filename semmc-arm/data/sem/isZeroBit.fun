((function isZeroBit)
 (arguments
  ((x
   (BV 32))))
 (return
  (BV 1))
 (body
  (with
   ()
   (ite
    (bveq op.x #x00000000)
    #b1
    #b0))))
