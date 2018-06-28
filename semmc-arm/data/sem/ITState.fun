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
     ((_ extract 11 10)
      cpsr)
     ((_ extract 26 25)
      cpsr))
    #x0)
   #xe
   ((_ extract 15 12)
    cpsr))))
