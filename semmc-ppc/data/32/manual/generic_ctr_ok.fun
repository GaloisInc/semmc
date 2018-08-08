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
    (bveq
     #b1
     ((_ extract 0 0)
      (bvlshr bo #b00010)))
    (true)
    (ite
     (bveq
      #b1
      ((_ extract 0 0)
       (bvlshr bo #b00001)))
     (xorp
      (notp
       (bveq newCtr #x00000000))
      (true))
     (xorp
      (notp
       (bveq newCtr #x00000000))
      (false)))))))
