((function updateCRField)
 (arguments
  ((cr
   (BV 32))
   (fldNum
    (BV 3))
   (newFldVal
    (BV 4))))
 (return
  (BV 32))
 (body
  (with
   ()
   (bvor
    (bvand
     op.cr
     (bvnot
      (bvshl
       #x0000000f
       (bvmul
        (bvsub
         #x00000007
         ((_ zero_extend 29)
          op.fldNum))
        #x00000004))))
    (bvshl
     ((_ zero_extend 28)
      op.newFldVal)
     (bvmul
      (bvsub
       #x00000007
       ((_ zero_extend 29)
        op.fldNum))
      #x00000004))))))
