(let
 ((conditionMatch
  (ite
   (bveq
    ((_ extract 2 0)
     instrPred)
    #b000)
   (bveq
    #b1
    ((_ extract 1 1)
     cpsr))
   (ite
    (bveq
     ((_ extract 2 0)
      instrPred)
     #b001)
    (bveq
     #b1
     ((_ extract 2 2)
      cpsr))
    (ite
     (bveq
      ((_ extract 2 0)
       instrPred)
      #b010)
     (bveq
      #b1
      ((_ extract 0 0)
       cpsr))
     (ite
      (bveq
       ((_ extract 2 0)
        instrPred)
       #b011)
      (bveq
       #b1
       ((_ extract 3 3)
        cpsr))
      (ite
       (bveq
        ((_ extract 2 0)
         instrPred)
        #b100)
       (andp
        (bveq
         #b1
         ((_ extract 2 2)
          cpsr))
        (notp
         (bveq
          #b1
          ((_ extract 1 1)
           cpsr))))
       (ite
        (bveq
         ((_ extract 2 0)
          instrPred)
         #b101)
        (bveq
         ((_ extract 0 0)
          cpsr)
         ((_ extract 3 3)
          cpsr))
        (ite
         (bveq
          ((_ extract 2 0)
           instrPred)
          #b110)
         (andp
          (bveq
           ((_ extract 0 0)
            cpsr)
           ((_ extract 3 3)
            cpsr))
          (notp
           (bveq
            #b1
            ((_ extract 1 1)
             cpsr))))
         (true)))))))))
  (true
   (bveq #b0 #b0)))
 ((function testCondition)
  (arguments
   ((instrPred
    ('bv 4))
    (cpsr
     ('bv 32))))
  (return 'bool)
  (body
   (ite
    (andp
     (bveq
      #b1
      ((_ extract 3 3)
       instrPred))
     (bvne instrPred #xf))
    (notp
     (conditionMatch))
    (conditionMatch)))))
