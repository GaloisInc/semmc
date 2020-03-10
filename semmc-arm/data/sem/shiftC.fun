((function shiftC)
 (arguments
  ((value
   (BV 32))
   (shift_t
    (BV 3))
   (shift_n
    (BV 32))
   (c
    (BV 1))))
 (return
  (BV 33))
 (body
  (with
   ()
   (let
    ((lslC
     (bvshl
      ((_ zero_extend 1)
       op.value)
      ((_ zero_extend 1)
       op.shift_n)))
     (logicalShiftRightCarry
      (concat
       (ite
        (bvult op.shift_n #x00000020)
        (ite
         (bveq
          #b1
          ((_ extract 0 0)
           (bvlshr
            (bvsub op.shift_n #x00000001)
            op.value)))
         #b1
         #b0)
        #b0)
       (bvlshr
        op.value
        (bvurem op.shift_n #x00000020))))
     (arithmeticShiftRightCarry
      (concat
       (ite
        (bvult op.shift_n #x00000020)
        (ite
         (bveq
          #b1
          ((_ extract 0 0)
           (bvlshr
            (bvsub op.shift_n #x00000001)
            op.value)))
         #b1
         #b0)
        #b0)
       (bvashr
        op.value
        (bvurem op.shift_n #x00000020))))
     (rorC
      (concat
       (ite
        (bvult op.shift_n #x00000020)
        (ite
         (bveq
          #b1
          ((_ extract 0 0)
           (bvlshr
            (bvsub op.shift_n #x00000001)
            op.value)))
         #b1
         #b0)
        #b0)
       (bvor
        (bvshl
         op.value
         (bvsub
          #x00000020
          (bvurem op.shift_n #x00000020)))
        (bvlshr
         op.value
         (bvurem op.shift_n #x00000020)))))
     (rrxC
      (concat
       ((_ extract 31 31)
        op.value)
       (concat
        op.c
        ((_ extract 30 0)
         op.value)))))
    (ite
     (bveq op.shift_n #x00000000)
     (concat op.c op.value)
     (ite
      (bveq op.shift_t #b000)
      lslC
      (ite
       (bveq op.shift_t #b001)
       logicalShiftRightCarry
       (ite
        (bveq op.shift_t #b010)
        arithmeticShiftRightCarry
        (ite
         (bveq op.shift_t #b011)
         rorC
         rrxC)))))))))
