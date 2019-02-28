(let
 ((addCarry
  (concat
   ((_ extract 0 0)
    ((_ extract 32 1)
     (bvadd
      (bvadd
       ((_ zero_extend 1)
        x)
       ((_ zero_extend 1)
        y))
      ((_ zero_extend 1)
       carry_in))))
   (concat
    (ite
     (bveq
      ((_ extract 32 1)
       (bvadd
        (bvadd
         ((_ zero_extend 1)
          x)
         ((_ zero_extend 1)
          y))
        ((_ zero_extend 1)
         carry_in)))
      #x00000000)
     #b1
     #b0)
    (concat
     ((_ extract 0 0)
      (bvadd
       (bvadd
        ((_ zero_extend 1)
         x)
        ((_ zero_extend 1)
         y))
       ((_ zero_extend 1)
        carry_in)))
     (bvand
      ((_ extract 0 0)
       ((_ extract 32 1)
        (bvadd
         (bvadd
          ((_ zero_extend 1)
           x)
          ((_ zero_extend 1)
           y))
         ((_ zero_extend 1)
          carry_in))))
      ((_ extract 0 0)
       (bvadd
        (bvadd
         ((_ zero_extend 1)
          x)
         ((_ zero_extend 1)
          y))
        ((_ zero_extend 1)
         carry_in))))))))
  (addResult
   ((_ extract 32 1)
    (bvadd
     (bvadd
      ((_ zero_extend 1)
       x)
      ((_ zero_extend 1)
       y))
     ((_ zero_extend 1)
      carry_in)))))
 ((function addWithCarry)
  (arguments
   ((x
    ('bv 32))
    (y
     ('bv 32))
    (carry_in
     ('bv 32))))
  (return
   ('bv 36))
  (body
   (concat
    (addResult)
    (addCarry)))))
