(let
 ((var1
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
       ((_ extract 1 0)
        imm12)
       #b00)
      (ite
       (bveq
        ((_ extract 3 2)
         imm12)
        #b00)
       ((_ zero_extend 24)
        ((_ extract 11 4)
         imm12))
       (ite
        (bveq
         ((_ extract 3 2)
          imm12)
         #b01)
        (ite
         (bveq
          ((_ extract 11 4)
           imm12)
          #x00)
         ((_ extract 32 1)
          (rorC))
         (concat
          (concat
           (concat
            #x00
            ((_ extract 11 4)
             imm12))
           #x00)
          ((_ extract 11 4)
           imm12)))
        (ite
         (bveq
          ((_ extract 3 2)
           imm12)
          #b10)
         (ite
          (bveq
           ((_ extract 11 4)
            imm12)
           #x00)
          ((_ extract 32 1)
           (rorC))
          (concat
           (concat
            (concat
             ((_ extract 11 4)
              imm12)
             #x00)
            ((_ extract 11 4)
             imm12))
           #x00))
         (ite
          (bveq
           ((_ extract 11 4)
            imm12)
           #x00)
          ((_ extract 32 1)
           (rorC))
          (concat
           (concat
            (concat
             ((_ extract 11 4)
              imm12)
             ((_ extract 11 4)
              imm12))
            ((_ extract 11 4)
             imm12))
           ((_ extract 11 4)
            imm12))))))
      ((_ extract 32 1)
       (rorC)))
     (ite
      (bveq
       ((_ extract 1 0)
        imm12)
       #b00)
      carry_in
      ((_ extract 0 0)
       (rorC)))))))
  (rorC
   (concat
    (ite
     (bvult
      ((_ zero_extend 27)
       ((_ extract 4 0)
        imm12))
      #x00000020)
     (ite
      (bveq
       #b1
       ((_ extract 0 0)
        (bvlshr
         (bvsub
          ((_ zero_extend 27)
           ((_ extract 4 0)
            imm12))
          #x00000001)
         ((_ zero_extend 24)
          (concat
           #b1
           ((_ extract 11 5)
            imm12))))))
      #b1
      #b0)
     #b0)
    (bvor
     (bvshl
      ((_ zero_extend 24)
       (concat
        #b1
        ((_ extract 11 5)
         imm12)))
      (bvsub
       #x00000020
       (bvurem
        ((_ zero_extend 27)
         ((_ extract 4 0)
          imm12))
        #x00000020)))
     (bvlshr
      ((_ zero_extend 24)
       (concat
        #b1
        ((_ extract 11 5)
         imm12)))
      (bvurem
       ((_ zero_extend 27)
        ((_ extract 4 0)
         imm12))
       #x00000020))))))
 (var1))
