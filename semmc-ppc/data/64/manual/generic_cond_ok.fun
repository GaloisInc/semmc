(let
 ((true
  (bveq #b0 #b0)))
 ((function generic_cond_ok)
  (arguments
   ((cr
    ('bv 32))
    (bo
     ('bv 5))
    (bi
     ('bv 5))))
  (return 'bool)
  (body
   (ite
    ((_ call "uf.test_bit_dynamic")
     #x00000000
     ((_ zero_extend 27)
      bo))
    (true)
    (ite
     ((_ call "uf.test_bit_dynamic")
      #x00000001
      ((_ zero_extend 27)
       bo))
     ((_ call "uf.test_bit_dynamic")
      ((_ zero_extend 27)
       bi)
      cr)
     (notp
      ((_ call "uf.test_bit_dynamic")
       ((_ zero_extend 27)
        bi)
       cr)))))))
