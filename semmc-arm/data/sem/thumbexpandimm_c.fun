(let
 ((rorC
  (concat
   (ite
    (bvult
     ((_ zero_extend 27)
      ((_ extract 11 7)
       imm12))
     #x00000020)
    (ite
     ((_ call "uf.test_bit_dynamic")
      (bvsub
       ((_ zero_extend 27)
        ((_ extract 11 7)
         imm12))
       #x00000001)
      ((_ zero_extend 24)
       (concat
        #b1
        ((_ extract 6 0)
         imm12))))
     #b1
     #b0)
    #b0)
   (bvor
    (bvshl
     ((_ zero_extend 24)
      (concat
       #b1
       ((_ extract 6 0)
        imm12)))
     (bvsub
      #x00000020
      (bvurem
       ((_ zero_extend 27)
        ((_ extract 11 7)
         imm12))
       #x00000020)))
    (bvlshr
     ((_ zero_extend 24)
      (concat
       #b1
       ((_ extract 6 0)
        imm12)))
     (bvurem
      ((_ zero_extend 27)
       ((_ extract 11 7)
        imm12))
      #x00000020))))))
 ((function thumbexpandimm_c)
  (arguments
   ((imm12
    ('bv 12))
    (carry_in
     ('bv 1))))
  (return
   ('bv 33))
  (body
   (concat
    (ite
     (bveq
      ((_ extract 11 10)
       imm12)
      #b00)
     (ite
      (bveq
       ((_ extract 9 8)
        imm12)
       #b00)
      ((_ zero_extend 24)
       ((_ extract 7 0)
        imm12))
      (ite
       (bveq
        ((_ extract 9 8)
         imm12)
        #b01)
       (ite
        (bveq
         ((_ extract 7 0)
          imm12)
         #x00)
        ((_ extract 31 0)
         (rorC))
        (concat
         (concat
          (concat
           #x00
           ((_ extract 7 0)
            imm12))
          #x00)
         ((_ extract 7 0)
          imm12)))
       (ite
        (bveq
         ((_ extract 9 8)
          imm12)
         #b10)
        (ite
         (bveq
          ((_ extract 7 0)
           imm12)
          #x00)
         ((_ extract 31 0)
          (rorC))
         (concat
          (concat
           (concat
            ((_ extract 7 0)
             imm12)
            #x00)
           ((_ extract 7 0)
            imm12))
          #x00))
        (ite
         (bveq
          ((_ extract 7 0)
           imm12)
          #x00)
         ((_ extract 31 0)
          (rorC))
         (concat
          (concat
           (concat
            ((_ extract 7 0)
             imm12)
            ((_ extract 7 0)
             imm12))
           ((_ extract 7 0)
            imm12))
          ((_ extract 7 0)
           imm12))))))
     ((_ extract 31 0)
      (rorC)))
    (ite
     (bveq
      ((_ extract 11 10)
       imm12)
      #b00)
     carry_in
     ((_ extract 32 32)
      (rorC)))))))
