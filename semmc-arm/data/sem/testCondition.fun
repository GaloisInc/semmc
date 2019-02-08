(let
 ((conditionMatch
  (ite
   (bveq
    ((_ xtract 3 1)
     instrPred)
    #b000)
   (bveq
    #b1
    ((_ xtract 30 30)
     cpsr))
   (ite
    (bveq
     ((_ xtract 3 1)
      instrPred)
     #b001)
    (bveq
     #b1
     ((_ xtract 29 29)
      cpsr))
    (ite
     (bveq
      ((_ xtract 3 1)
       instrPred)
      #b010)
     (bveq
      #b1
      ((_ xtract 31 31)
       cpsr))
     (ite
      (bveq
       ((_ xtract 3 1)
        instrPred)
       #b011)
      (bveq
       #b1
       ((_ xtract 28 28)
        cpsr))
      (ite
       (bveq
        ((_ xtract 3 1)
         instrPred)
        #b100)
       (andp
        (bveq
         #b1
         ((_ xtract 29 29)
          cpsr))
        (notp
         (bveq
          #b1
          ((_ xtract 30 30)
           cpsr))))
       (ite
        (bveq
         ((_ xtract 3 1)
          instrPred)
         #b101)
        (bveq
         ((_ xtract 31 31)
          cpsr)
         ((_ xtract 28 28)
          cpsr))
        (ite
         (bveq
          ((_ xtract 3 1)
           instrPred)
          #b110)
         (andp
          (bveq
           ((_ xtract 31 31)
            cpsr)
           ((_ xtract 28 28)
            cpsr))
          (notp
           (bveq
            #b1
            ((_ xtract 30 30)
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
      ((_ xtract 0 0)
       instrPred))
     (bvne instrPred #xf))
    (notp
     (conditionMatch))
    (conditionMatch)))))
