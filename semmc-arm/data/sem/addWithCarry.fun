((function addWithCarry)
 (arguments
  ((x
   (BV 32))
   (y
    (BV 32))
   (carry_in
    (BV 32))))
 (return
  (BV 36))
 (body
  (with
   ()
   (let
    ((addResult
     ((_ extract 32 1)
      (bvadd
       (bvadd
        ((_ zero_extend 1)
         op.x)
        ((_ zero_extend 1)
         op.y))
       ((_ zero_extend 1)
        op.carry_in))))
     (addCarry
      (concat
       ((_ extract 0 0)
        ((_ extract 32 1)
         (bvadd
          (bvadd
           ((_ zero_extend 1)
            op.x)
           ((_ zero_extend 1)
            op.y))
          ((_ zero_extend 1)
           op.carry_in))))
       (concat
        (ite
         (bveq
          ((_ extract 32 1)
           (bvadd
            (bvadd
             ((_ zero_extend 1)
              op.x)
             ((_ zero_extend 1)
              op.y))
            ((_ zero_extend 1)
             op.carry_in)))
          #x00000000)
         #b1
         #b0)
        (concat
         ((_ extract 0 0)
          (bvadd
           (bvadd
            ((_ zero_extend 1)
             op.x)
            ((_ zero_extend 1)
             op.y))
           ((_ zero_extend 1)
            op.carry_in)))
         (bvand
          ((_ extract 0 0)
           ((_ extract 32 1)
            (bvadd
             (bvadd
              ((_ zero_extend 1)
               op.x)
              ((_ zero_extend 1)
               op.y))
             ((_ zero_extend 1)
              op.carry_in))))
          ((_ extract 0 0)
           (bvadd
            (bvadd
             ((_ zero_extend 1)
              op.x)
             ((_ zero_extend 1)
              op.y))
            ((_ zero_extend 1)
             op.carry_in)))))))))
    (concat addResult addCarry)))))
