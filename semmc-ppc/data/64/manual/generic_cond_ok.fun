((function generic_cond_ok)
 (arguments
  ((cr
   (BV 32))
   (bo
    (BV 5))
   (bi
    (BV 5))))
 (return Bool)
 (body
  (with
   ()
   (let
    ((true
     (bveq #b0 #b0)))
    (ite
     (bveq
      #b1
      ((_ extract 0 0)
       (bvlshr op.bo #b00100)))
     true
     (ite
      (bveq
       #b1
       ((_ extract 0 0)
        (bvlshr op.bo #b00011)))
      (bveq
       #b1
       ((_ extract 0 0)
        (bvlshr
         op.cr
         ((_ zero_extend 27)
          (bvsub #b11111 op.bi)))))
      (notp
       (bveq
        #b1
        ((_ extract 0 0)
         (bvlshr
          op.cr
          ((_ zero_extend 27)
           (bvsub #b11111 op.bi))))))))))))
