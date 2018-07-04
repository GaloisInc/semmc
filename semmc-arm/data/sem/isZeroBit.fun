((function isZeroBit)
 (arguments
  ((x
   ('bv 32))))
 (return
  ('bv 1))
 (body
  (ite
   (bveq x #x00000000)
   #b1
   #b0)))
