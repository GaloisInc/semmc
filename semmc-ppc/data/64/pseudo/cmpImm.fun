((function cmpImm)
 (arguments
  ((lt Bool)
   (gt Bool)
   (fld
    (BV 3))
   (xer
    (BV 64))
   (cr
    (BV 32))))
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
          op.fld))
        #x00000004))))
    (bvshl
     ((_ zero_extend 28)
      (concat
       (ite
        op.lt
        #b100
        (ite op.gt #b010 #b001))
       ((_ extract 31 31)
        op.xer)))
     (bvmul
      (bvsub
       #x00000007
       ((_ zero_extend 29)
        op.fld))
      #x00000004))))))
