((function testCondition)
 (arguments
  ((instrPred
   (BV 4))
   (cpsr
    (BV 32))))
 (return Bool)
 (body
  (with
   ()
   (let
    ((true
     (bveq #b0 #b0))
     (conditionMatch
      (ite
       (bveq
        ((_ extract 2 0)
         op.instrPred)
        #b000)
       (bveq
        #b1
        ((_ extract 1 1)
         op.cpsr))
       (ite
        (bveq
         ((_ extract 2 0)
          op.instrPred)
         #b001)
        (bveq
         #b1
         ((_ extract 2 2)
          op.cpsr))
        (ite
         (bveq
          ((_ extract 2 0)
           op.instrPred)
          #b010)
         (bveq
          #b1
          ((_ extract 0 0)
           op.cpsr))
         (ite
          (bveq
           ((_ extract 2 0)
            op.instrPred)
           #b011)
          (bveq
           #b1
           ((_ extract 3 3)
            op.cpsr))
          (ite
           (bveq
            ((_ extract 2 0)
             op.instrPred)
            #b100)
           (andp
            (bveq
             #b1
             ((_ extract 2 2)
              op.cpsr))
            (notp
             (bveq
              #b1
              ((_ extract 1 1)
               op.cpsr))))
           (ite
            (bveq
             ((_ extract 2 0)
              op.instrPred)
             #b101)
            (bveq
             ((_ extract 0 0)
              op.cpsr)
             ((_ extract 3 3)
              op.cpsr))
            (ite
             (bveq
              ((_ extract 2 0)
               op.instrPred)
              #b110)
             (andp
              (bveq
               ((_ extract 0 0)
                op.cpsr)
               ((_ extract 3 3)
                op.cpsr))
              (notp
               (bveq
                #b1
                ((_ extract 1 1)
                 op.cpsr))))
             true)))))))))
    (ite
     (andp
      (bveq
       #b1
       ((_ extract 3 3)
        op.instrPred))
      (bvne op.instrPred #xf))
     (notp conditionMatch)
     conditionMatch)))))
