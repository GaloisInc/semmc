((function mask_64)
 (arguments
  ((b0
   ('bv 64))
   (b1
    ('bv 64))))
 (return
  ('bv 64))
 (body
  (bvshl
   (bvlshr
    (bvlshr
     (bvshl
      ((_ sign_extend 63)
       #b1)
      b0)
     b0)
    (bvsub #x000000000000003f b1))
   (bvsub #x000000000000003f b1))))
