((function ITState)
 (arguments
  ((cpsr
   (BV 32))))
 (return
  (BV 4))
 (body
  (with
   ()
   (ite
    (bveq
     (concat
      ((_ extract 21 20)
       op.cpsr)
      ((_ extract 6 5)
       op.cpsr))
     #x0)
    #xe
    ((_ extract 19 16)
     op.cpsr)))))
