((function cmpImm)
 (arguments
  ((lt 'bool)
   (gt 'bool)
   (fld
    ('bv 3))
   (xer
    ('bv 32))
   (cr
    ('bv 32))))
 (return
  ('bv 32))
 (body
  (bvor
   (bvand
    cr
    (bvnot
     (bvshl
      #x0000000f
      (bvmul
       (bvsub
        #x00000007
        ((_ zero_extend 29)
         fld))
       #x00000004))))
   (bvshl
    ((_ zero_extend 28)
     (concat
      (ite
       lt
       #b100
       (ite gt #b010 #b001))
      ((_ extract 31 31)
       xer)))
    (bvmul
     (bvsub
      #x00000007
      ((_ zero_extend 29)
       fld))
     #x00000004)))))
