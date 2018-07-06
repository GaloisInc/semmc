((function mask_32)
 (arguments
  ((b0
   ('bv 32))
   (b1
    ('bv 32))))
 (return
  ('bv 32))
 (body
  (ite
   (bvule b0 b1)
   (bvshl
    (bvlshr
     (bvlshr
      (bvshl
       ((_ sign_extend 31)
        #b1)
       b0)
      b0)
     (bvsub #x0000001f b1))
    (bvsub #x0000001f b1))
   (bvnot
    (bvshl
     (bvlshr
      (bvlshr
       (bvshl
        ((_ sign_extend 31)
         #b1)
        (bvadd b1 #x00000001))
       (bvadd b1 #x00000001))
      (bvsub
       #x0000001f
       (bvsub b0 #x00000001)))
     (bvsub
      #x0000001f
      (bvsub b0 #x00000001)))))))
