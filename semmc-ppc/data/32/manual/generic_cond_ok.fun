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
     ((_ zero_extend 27)
      bo)
     #x00000004)
    (true)
    (ite
     ((_ call "uf.test_bit_dynamic")
      ((_ zero_extend 27)
       bo)
      #x00000003)
     ((_ call "uf.test_bit_dynamic")
      cr
      ((_ zero_extend 27)
       (bvsub #b11111 bi)))
     (notp
      ((_ call "uf.test_bit_dynamic")
       cr
       ((_ zero_extend 27)
        (bvsub #b11111 bi)))))))))
