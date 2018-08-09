(let
 ((rrxC
  (concat
   ((_ extract 31 31)
    value)
   (concat
    c
    ((_ extract 30 0)
     value))))
  (rorC
   (concat
    (ite
     (bvult shift_n #x00000020)
     (ite
      (bveq
       #b1
       ((_ extract 0 0)
        (bvlshr
         (bvsub shift_n #x00000001)
         value)))
      #b1
      #b0)
     #b0)
    (bvor
     (bvshl
      value
      (bvsub
       #x00000020
       (bvurem shift_n #x00000020)))
     (bvlshr
      value
      (bvurem shift_n #x00000020)))))
  (arithmeticShiftRightCarry
   (concat
    (ite
     (bvult shift_n #x00000020)
     (ite
      (bveq
       #b1
       ((_ extract 0 0)
        (bvlshr
         (bvsub shift_n #x00000001)
         value)))
      #b1
      #b0)
     #b0)
    (bvashr
     value
     (bvurem shift_n #x00000020))))
  (logicalShiftRightCarry
   (concat
    (ite
     (bvult shift_n #x00000020)
     (ite
      (bveq
       #b1
       ((_ extract 0 0)
        (bvlshr
         (bvsub shift_n #x00000001)
         value)))
      #b1
      #b0)
     #b0)
    (bvlshr
     value
     (bvurem shift_n #x00000020))))
  (lslC
   (bvshl
    ((_ zero_extend 1)
     value)
    ((_ zero_extend 1)
     shift_n))))
 ((function shiftC)
  (arguments
   ((value
    ('bv 32))
    (shift_t
     ('bv 3))
    (shift_n
     ('bv 32))
    (c
     ('bv 1))))
  (return
   ('bv 33))
  (body
   (ite
    (bveq shift_n #x00000000)
    (concat c value)
    (ite
     (bveq shift_t #b000)
     (lslC)
     (ite
      (bveq shift_t #b001)
      (logicalShiftRightCarry)
      (ite
       (bveq shift_t #b010)
       (arithmeticShiftRightCarry)
       (ite
        (bveq shift_t #b011)
        (rorC)
        (rrxC)))))))))
