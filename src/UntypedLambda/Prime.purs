module UntypedLambda.Prime
  ( prime
  ) where

prime :: String
prime =
  """zero  = λf x.x
one   = λf x.f x
two   = λf x.f (f x)
three = λf x.f (f (f x))
four  = λf x.f (f (f (f x)))
five  = λf x.f (f (f (f (f x))))
six   = λf x.f (f (f (f (f (f x)))))
seven = λf x.f (f (f (f (f (f (f x))))))
eight = λf x.f (f (f (f (f (f (f (f x)))))))
nine  = λf x.f (f (f (f (f (f (f (f (f x))))))))
ten   = λf x.f (f (f (f (f (f (f (f (f (f x)))))))))
succ = λn f x.f (n f x)
plus = λm n f x.m f (n f x)
mult = λm n f.m (n f)
pow  = λb e.e b
pred = λn f x.n (λg h.h (g f)) (λu.x) (λu.u)
sub  = λm n.n pred m"""
