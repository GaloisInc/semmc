((function mask_32)
 (arguments
  ((b0
   (BV 32))
   (b1
    (BV 32))))
 (return
  (BV 32))
 (body
  (with
   ()
   (ite
    (bvule op.b0 op.b1)
    (bvshl
     (bvlshr
      (bvlshr
       (bvshl
        ((_ sign_extend 31)
         #b1)
        op.b0)
       op.b0)
      (bvsub #x0000001f op.b1))
     (bvsub #x0000001f op.b1))
    (bvnot
     (bvshl
      (bvlshr
       (bvlshr
        (bvshl
         ((_ sign_extend 31)
          #b1)
         (bvadd op.b1 #x00000001))
        (bvadd op.b1 #x00000001))
       (bvsub
        #x0000001f
        (bvsub op.b0 #x00000001)))
      (bvsub
       #x0000001f
       (bvsub op.b0 #x00000001))))))))
