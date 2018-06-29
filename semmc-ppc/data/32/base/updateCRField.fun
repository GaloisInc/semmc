((function updateCRField)
 (arguments
  ((cr
   ('bv 32))
   (fldNum
    ('bv 3))
   (newFldVal
    ('bv 4))))
 (return
  ('bv 32))
 (body
  (bvor
   (bvand
    cr
    (bvnot
     (bvshl
      #x0000000f
      (bvmul
       ((_ zero_extend 29)
        fldNum)
       #x00000004))))
   (bvshl
    ((_ zero_extend 28)
     newFldVal)
    (bvmul
     ((_ zero_extend 29)
      fldNum)
     #x00000004)))))
