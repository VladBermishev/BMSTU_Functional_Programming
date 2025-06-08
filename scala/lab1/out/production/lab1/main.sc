val peaks: List[Int] => List[Int] = {
  val __kek: List[Int] => List[Int] = {
    case Nil => Nil;
    case current :: Nil => List(1);
    case prev :: current :: Nil => List(if (current >= prev) 1 else 0);
    case prev :: current :: next :: tail => (if (current >= prev && current >= next) 1 else 0) :: __kek(current :: next :: tail);
  }
  {
    case current :: next :: tail => (if (current >= next) 1 else 0) :: __kek(current :: next :: tail);
    case default => __kek(default)
  }
}
peaks(1 :: 2 :: 3 :: 4 :: 5 :: Nil) // -> List(0,0,0,0,1)
peaks(5 :: 4 :: 3 :: 2 :: 1 :: Nil) // -> List(1,0,0,0,0)
peaks(Nil) // -> Nil
peaks(List(1)) // -> List(1)
