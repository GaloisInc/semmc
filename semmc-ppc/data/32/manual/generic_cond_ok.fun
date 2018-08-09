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
    (bveq
     #b1
     ((_ extract 0 0)
      (bvlshr bo #b00100)))
    (true)
    (ite
     (bveq
      #b1
      ((_ extract 0 0)
       (bvlshr bo #b00011)))
     (bveq
      #b1
      ((_ extract 0 0)
       (bvlshr
        cr
        ((_ zero_extend 27)
         (bvsub #b11111 bi)))))
     (notp
      (bveq
       #b1
       ((_ extract 0 0)
        (bvlshr
         cr
         ((_ zero_extend 27)
          (bvsub #b11111 bi)))))))))))
