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
   (ite
    (bveq
     #b1
     ((_ extract 0 0)
      (bvlshr op.bo #b00010)))
    #true
    (ite
     (bveq
      #b1
      ((_ extract 0 0)
       (bvlshr op.bo #b00001)))
     (xorp
      (notp
       (bveq op.newCtr #x0000000000000000))
      #true)
     (xorp
      (notp
       (bveq op.newCtr #x0000000000000000))
      #false))))))
