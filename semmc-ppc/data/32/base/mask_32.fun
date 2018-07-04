((function mask_32)
 (arguments
  ((b0
   ('bv 32))
   (b1
    ('bv 32))))
 (return
  ('bv 32))
 (body
  (bvshl
   (bvlshr
    (bvlshr
     (bvshl
      ((_ sign_extend 31)
       #b1)
      b0)
     b0)
    (bvsub #x0000001f b1))
   (bvsub #x0000001f b1))))
