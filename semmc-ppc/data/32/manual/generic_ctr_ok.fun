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
     ('bv 32))))
  (return 'bool)
  (body
   (ite
    ((_ call "uf.test_bit_dynamic")
     #x00000002
     ((_ zero_extend 27)
      bo))
    (true)
    (ite
     ((_ call "uf.test_bit_dynamic")
      #x00000003
      ((_ zero_extend 27)
       bo))
     (xorp
      (notp
       (bveq newCtr #x00000000))
      (true))
     (xorp
      (notp
       (bveq newCtr #x00000000))
      (false)))))))
