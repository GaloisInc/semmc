((function mask_64)
 (arguments
  ((b0
   ('bv 64))
   (b1
    ('bv 64))))
 (return
  ('bv 64))
 (body
  (ite
   (bvule b0 b1)
   (bvshl
    (bvlshr
     (bvlshr
      (bvshl
       ((_ sign_extend 63)
        #b1)
       b0)
      b0)
     (bvsub #x000000000000003f b1))
    (bvsub #x000000000000003f b1))
   (bvnot
    (bvshl
     (bvlshr
      (bvlshr
       (bvshl
        ((_ sign_extend 63)
         #b1)
        (bvadd b1 #x0000000000000001))
       (bvadd b1 #x0000000000000001))
      (bvsub
       #x000000000000003f
       (bvsub b0 #x0000000000000001)))
     (bvsub
      #x000000000000003f
      (bvsub b0 #x0000000000000001)))))))
