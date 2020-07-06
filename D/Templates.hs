module D.Templates where
  import qualified Data.Set as Set

  import D.Common
  import D.Types

  excludedMiddleLaw :: Exp -> [Exp]
  excludedMiddleLaw x = first ++ second ++ [
      (EImpl (EImpl (ENeg (EDisj x (ENeg x))) (ENeg x)) (EImpl (EImpl (ENeg (EDisj x (ENeg x))) (ENeg (ENeg x))) (ENeg (ENeg (EDisj x (ENeg x)))))),
      (EImpl (EImpl (ENeg (EDisj x (ENeg x))) (ENeg (ENeg x))) (ENeg (ENeg (EDisj x (ENeg x))))),
      (ENeg (ENeg (EDisj x (ENeg x)))),
      (EImpl (ENeg (ENeg (EDisj x (ENeg x)))) (EDisj x (ENeg x))),
      (EDisj x (ENeg x))
    ]
    where
      first = (EImpl x (EDisj x (ENeg x))) : contraposition x (EDisj x (ENeg x)) ++ [
          (EImpl (ENeg (EDisj x (ENeg x))) (ENeg x))
        ]
      second = (EImpl (ENeg x) (EDisj x (ENeg x))) : contraposition (ENeg x) (EDisj x (ENeg x)) ++ [
          (EImpl (ENeg (EDisj x (ENeg x))) (ENeg (ENeg x)))
        ]

  contraposition :: Exp -> Exp -> [Exp]
  contraposition a b = unhypothesize (EImpl a b) Set.empty $ unhypothesize (ENeg b) (Set.singleton (EImpl a b)) [
      (EImpl (EImpl a b) (EImpl (EImpl a (ENeg b)) (ENeg a))),
      (EImpl a b),
      (EImpl (EImpl a (ENeg b)) (ENeg a)),
      (EImpl (ENeg b) (EImpl a (ENeg b))),
      (ENeg b),
      (EImpl a (ENeg b)),
      (ENeg a)
    ]

  -- !l, !r |- !(l & r)
  template1 :: Exp -> Exp -> [Exp]
  template1 l r = [
    (EImpl (ENeg l) (EImpl (EConj l r) (ENeg l))),
    (ENeg l),
    (EImpl (EConj l r) (ENeg l)),
    (EImpl (EConj l r) l),
    (EImpl (EImpl (EConj l r) l) (EImpl (EImpl (EConj l r) (ENeg l)) (ENeg (EConj l r)))),
    (EImpl (EImpl (EConj l r) (ENeg l)) (ENeg (EConj l r))),
    (ENeg (EConj l r))]

  -- !l, r |- !(l & r)
  template2 :: Exp -> Exp -> [Exp]
  template2 = template1

  -- l, !r |- !(l & r)
  template3 :: Exp -> Exp -> [Exp]
  template3 l r = [
    (EImpl (ENeg r) (EImpl (EConj l r) (ENeg r))),
    (ENeg r),
    (EImpl (EConj l r) (ENeg r)),
    (EImpl (EConj l r) r),
    (EImpl (EImpl (EConj l r) r) (EImpl (EImpl (EConj l r) (ENeg r)) (ENeg (EConj l r)))),
    (EImpl (EImpl (EConj l r) (ENeg r)) (ENeg (EConj l r))),
    (ENeg (EConj l r))]

  -- l, r |- l & r
  template4 :: Exp -> Exp -> [Exp]
  template4 l r = [
    -- l,
    -- r,
    (EImpl l (EImpl r (EConj l r))),
    (EImpl r (EConj l r)),
    (EConj l r)]

  -- !l, !r |- !(l | r)
  template5 :: Exp -> Exp -> [Exp]
  template5 l r = [
      (ENeg l),
      (ENeg r),
      (EImpl (ENeg l) (EImpl (EDisj l r) (ENeg l))),
      (EImpl (EDisj l r) (ENeg l)),
      (EImpl l (EImpl l l)),
      (EImpl (EImpl l (EImpl l l)) (EImpl (EImpl l (EImpl (EImpl l l) l)) (EImpl l l))),
      (EImpl (EImpl l (EImpl (EImpl l l) l)) (EImpl l l)),
      (EImpl l (EImpl (EImpl l l) l)),
      (EImpl l l),
      (EImpl (ENeg r) (EImpl (ENeg l) (ENeg r))),
      (EImpl (ENeg l) (ENeg r))
    ] ++ contraposition (ENeg l) (ENeg r) ++ template13 r ++ [
      (EImpl (EImpl r (ENeg (ENeg r))) (EImpl (EImpl r (EImpl (ENeg (ENeg r)) (ENeg (ENeg l)))) (EImpl r (ENeg (ENeg l))))),
      (EImpl (EImpl r (EImpl (ENeg (ENeg r)) (ENeg (ENeg l)))) (EImpl r (ENeg (ENeg l)))),
      (EImpl (EImpl (ENeg (ENeg r)) (ENeg (ENeg l))) (EImpl r (EImpl (ENeg (ENeg r)) (ENeg (ENeg l))))),
      (EImpl r (EImpl (ENeg (ENeg r)) (ENeg (ENeg l)))),
      (EImpl r (ENeg (ENeg l))),
      (EImpl (ENeg (ENeg l)) l),
      (EImpl (EImpl (ENeg (ENeg l)) l) (EImpl r (EImpl (ENeg (ENeg l)) l))),
      (EImpl r (EImpl (ENeg (ENeg l)) l)),
      (EImpl (EImpl r (ENeg (ENeg l))) (EImpl (EImpl r (EImpl (ENeg (ENeg l)) l)) (EImpl r l))),
      (EImpl (EImpl r (EImpl (ENeg (ENeg l)) l)) (EImpl r l)),
      (EImpl r l),
      (EImpl (EImpl l l) (EImpl (EImpl r l) (EImpl (EDisj l r) l))),
      (EImpl (EImpl r l) (EImpl (EDisj l r) l)),
      (EImpl (EDisj l r) l),
      (EImpl (EImpl (EDisj l r) l) (EImpl (EImpl (EDisj l r) (ENeg l)) (ENeg (EDisj l r)))),
      (EImpl (EImpl (EDisj l r) (ENeg l)) (ENeg (EDisj l r))),
      (ENeg (EDisj l r))
    ]

  -- !l, r |- l | r
  template6 :: Exp -> Exp -> [Exp]
  template6 l r = [
      r,
      (EImpl r (EDisj l r)),
      (EDisj l r)
    ]

  -- l, !r |- l | r
  template7 :: Exp -> Exp -> [Exp]
  template7 l r = [
      l,
      (EImpl l (EDisj l r)),
      (EDisj l r)
    ]

  -- l, r |- l | r
  template8 :: Exp -> Exp -> [Exp]
  template8 = template6

  -- !l, !r |- l -> r
  template9 :: Exp -> Exp -> [Exp]
  template9 l r = [
      l,
      (EImpl (ENeg l) (EImpl (ENeg r) (ENeg l))),
      (EImpl (ENeg r) (ENeg l))
    ] ++ contraposition (ENeg r) (ENeg l)

  -- !l, r |- l -> r
  template10 :: Exp -> Exp -> [Exp]
  template10 l r = [
      r,
      (EImpl r (EImpl l r)),
      (EImpl l r)
    ]

  -- l, !r |- !(l -> r)
  template11 :: Exp -> Exp -> [Exp]
  template11 l r = [
      (l),
      (ENeg (r)),
      (EImpl (EImpl l r) (EImpl (EImpl l r) (EImpl l r))),
      (EImpl (EImpl (EImpl l r) (EImpl (EImpl l r) (EImpl l r))) (EImpl (EImpl (EImpl l r) (EImpl (EImpl (EImpl l r) (EImpl l r)) (EImpl l r))) (EImpl (EImpl l r) (EImpl l r)))),
      (EImpl (EImpl (EImpl l r) (EImpl (EImpl (EImpl l r) (EImpl l r)) (EImpl l r))) (EImpl (EImpl l r) (EImpl l r))),
      (EImpl (EImpl l r) (EImpl (EImpl (EImpl l r) (EImpl l r)) (EImpl l r))),
      (EImpl (EImpl l r) (EImpl l r)),
      (EImpl (l) (EImpl (EImpl (l) (r)) (l))),
      (EImpl (EImpl (l) (r)) (l)),
      (EImpl (EImpl (EImpl (l) (r)) (l)) (EImpl (EImpl (EImpl (l) (r)) (EImpl (l) (r))) (EImpl (EImpl (l) (r)) (r)))),
      (EImpl (EImpl (EImpl (l) (r)) (EImpl (l) (r))) (EImpl (EImpl (l) (r)) (r))),
      (EImpl (EImpl (l) (r)) (r)),
      (EImpl (ENeg (r)) (EImpl (EImpl (l) (r)) (ENeg (r)))),
      (EImpl (EImpl (l) (r)) (ENeg (r))),
      (EImpl (EImpl (EImpl (l) (r)) (r)) (EImpl (EImpl (EImpl (l) (r)) (ENeg (r))) (ENeg (EImpl (l) (r))))),
      (EImpl (EImpl (EImpl (l) (r)) (ENeg (r))) (ENeg (EImpl (l) (r)))),
      (ENeg (EImpl (l) (r)))
    ]

  -- l, r |- l -> r
  template12 :: Exp -> Exp -> [Exp]
  template12 = template10

  -- a |- !!a
  template13 :: Exp -> [Exp]
  template13 a = [
      a,
      (EImpl a (EImpl (ENeg a) a)),
      (EImpl (ENeg a) a),
      (EImpl (ENeg a) (EImpl (ENeg a) (ENeg a))),
      (EImpl (ENeg a) (EImpl (EImpl (ENeg a) (ENeg a)) (ENeg a))),
      (EImpl (EImpl (ENeg a) (EImpl (ENeg a) (ENeg a))) (EImpl (EImpl (ENeg a) (EImpl (EImpl (ENeg a) (ENeg a)) (ENeg a))) (EImpl (ENeg a) (ENeg a)))),
      (EImpl (EImpl (ENeg a) (EImpl (EImpl (ENeg a) (ENeg a)) (ENeg a))) (EImpl (ENeg a) (ENeg a))),
      (EImpl (ENeg a) (ENeg a)),
      (EImpl (EImpl (ENeg a) a) (EImpl (EImpl (ENeg a) (ENeg a)) (ENeg (ENeg a)))),
      (EImpl (EImpl (ENeg a) (ENeg a)) (ENeg (ENeg a))),
      (ENeg (ENeg a))
    ]
