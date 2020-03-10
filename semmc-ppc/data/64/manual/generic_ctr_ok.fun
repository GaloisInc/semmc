((function generic_ctr_ok)
 (arguments
  ((bo
   (BV 5))
   (newCtr
    (BV 64))))
 (return Bool)
 (body
  (with
   ()
   (let
    ((false
     (bveq #b0 #b0))
     (true
      (bveq #b0 #b0)))
    (ite
     (bveq
      #b1
      ((_ extract 0 0)
       (bvlshr op.bo #b00010)))
     true
     (ite
      (bveq
       #b1
       ((_ extract 0 0)
        (bvlshr op.bo #b00001)))
      (xorp
       (notp
        (bveq op.newCtr #x0000000000000000))
       true)
      (xorp
       (notp
        (bveq op.newCtr #x0000000000000000))
       false)))))))
