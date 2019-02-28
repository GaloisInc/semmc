((function ITState)
 (arguments
  ((cpsr
   ('bv 32))))
 (return
  ('bv 4))
 (body
  (ite
   (bveq
    (concat
     ((_ extract 21 20)
      cpsr)
     ((_ extract 6 5)
      cpsr))
    #x0)
   #xe
   ((_ extract 19 16)
    cpsr))))
