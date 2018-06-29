(let
 ((false
  (bvne #b0 #b0))
  (true
   (bveq #b0 #b0)))
 ((function generic_ctr_ok)
  (arguments
   ((bo
    ('bv 5))
    (newCtr
     ('bv 64))))
  (return 'bool)
  (body
   (ite
    ((_ call "uf.test_bit_dynamic")
     ((_ zero_extend 27)
      bo)
     #x00000002)
    (true)
    (ite
     ((_ call "uf.test_bit_dynamic")
      ((_ zero_extend 27)
       bo)
      #x00000001)
     (xorp
      (notp
       (bveq newCtr #x0000000000000000))
      (true))
     (xorp
      (notp
       (bveq newCtr #x0000000000000000))
      (false)))))))
