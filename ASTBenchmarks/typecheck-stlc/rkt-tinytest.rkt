#lang gibbon

(require "rkt-typechecker.rkt")

(let ([e : Expr
(App
 (Lam
  (CONSPARAM
   (P
    (S 'g8643)
    (Lamt
     (CONSTYPE (Lamt (CONSTYPE (Int_) (NULLTYPE)) (Int_)) (NULLTYPE))
     (Lamt
      (CONSTYPE
       (Lamt (CONSTYPE (Lamt (CONSTYPE (Int_) (NULLTYPE)) (Int_)) (NULLTYPE)) (Bool_))
       (NULLTYPE))
      (Lamt
       (CONSTYPE
        (Lamt
         (CONSTYPE (Bool_) (NULLTYPE))
         (Lamt
          (CONSTYPE
           (Lamt (CONSTYPE (Lamt (CONSTYPE (Int_) (NULLTYPE)) (Bool_)) (NULLTYPE)) (Int_))
           (NULLTYPE))
          (Lamt (CONSTYPE (Bool_) (NULLTYPE)) (Bool_))))
        (NULLTYPE))
       (Lamt (CONSTYPE (Lamt (CONSTYPE (Bool_) (NULLTYPE)) (Bool_)) (NULLTYPE)) (Int_))))))
   (CONSPARAM (P (S 'g8644) (Int_)) (NULLPARAM)))
  (Begin
   (CONSEXPR
    (B #f)
    (CONSEXPR
     (N 57)
     (CONSEXPR
      (N 62)
      (CONSEXPR
       (B #f)
       (CONSEXPR
        (B #t)
        (CONSEXPR
         (B #f)
         (CONSEXPR
          (B #t)
          (CONSEXPR
           (B #t)
           (CONSEXPR
            (N 11)
            (CONSEXPR
             (B #f)
             (CONSEXPR
              (N 53)
              (CONSEXPR
               (B #t)
               (CONSEXPR
                (B #t)
                (CONSEXPR
                 (B #f)
                 (CONSEXPR
                  (N 4)
                  (CONSEXPR
                   (N 66)
                   (CONSEXPR
                    (B #f)
                    (CONSEXPR
                     (N 52)
                     (CONSEXPR (B #t) (CONSEXPR (B #t) (NULLEXPR)))))))))))))))))))))))
 (CONSEXPR
  (Lam
   (CONSPARAM (P (S 'g8645) (Lamt (CONSTYPE (Int_) (NULLTYPE)) (Int_))) (NULLPARAM))
   (Lam
    (CONSPARAM
     (P
      (S 'g8646)
      (Lamt (CONSTYPE (Lamt (CONSTYPE (Int_) (NULLTYPE)) (Int_)) (NULLTYPE)) (Bool_)))
     (NULLPARAM))
    (Lam
     (CONSPARAM
      (P
       (S 'g8647)
       (Lamt
        (CONSTYPE (Bool_) (NULLTYPE))
        (Lamt
         (CONSTYPE
          (Lamt (CONSTYPE (Lamt (CONSTYPE (Int_) (NULLTYPE)) (Bool_)) (NULLTYPE)) (Int_))
          (NULLTYPE))
         (Lamt (CONSTYPE (Bool_) (NULLTYPE)) (Bool_)))))
      (NULLPARAM))
     (Lam
      (CONSPARAM (P (S 'g8648) (Lamt (CONSTYPE (Bool_) (NULLTYPE)) (Bool_))) (NULLPARAM))
      (N 87)))))
  (CONSEXPR (N 10) (NULLEXPR))))
])
  (iterate (test-typecheck e)))
   ;; (Begin (CONSEXPR e (CONSEXPR e (CONSEXPR e
   ;; 					    (CONSEXPR e (CONSEXPR e (CONSEXPR e (NULLEXPR))))))))


