((function mask_64)
 (arguments
  ((b0
   (BV 64))
   (b1
    (BV 64))))
 (return
  (BV 64))
 (body
  (with
   ()
   (ite
    (bvule op.b0 op.b1)
    (bvshl
     (bvlshr
      (bvlshr
       (bvshl
        ((_ sign_extend 63)
         #b1)
        op.b0)
       op.b0)
      (bvsub #x000000000000003f op.b1))
     (bvsub #x000000000000003f op.b1))
    (bvnot
     (bvshl
      (bvlshr
       (bvlshr
        (bvshl
         ((_ sign_extend 63)
          #b1)
         (bvadd op.b1 #x0000000000000001))
        (bvadd op.b1 #x0000000000000001))
       (bvsub
        #x000000000000003f
        (bvsub op.b0 #x0000000000000001)))
      (bvsub
       #x000000000000003f
       (bvsub op.b0 #x0000000000000001))))))))
