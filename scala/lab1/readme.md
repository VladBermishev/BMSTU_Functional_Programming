% Лабораторная работа № 1 «Введение в функциональное
программирование на языке Scala»
% 15 февраля 2023 г.
% Владислав Бермишев, ИУ9-62Б

# Цель работы
Целью данной работы является ознакомление с программированием на языке Scala на основе
чистых функций.

# Индивидуальный вариант
Функция peaks: List[Int] => List[Int], формирующая список индексов пиков последовательности
(пик — такой элемент, что соседние элементы его не превышают).

# Реализация и тестирование

Работа в REPL-интерпретаторе Scala:

```scala
val peaks: List[Int] => List[Int] = {
  lazy val __peaks: (List[Int], Int) => List[Int] = (lst, idx) => lst match {
    case Nil => Nil;
    case current :: Nil => List(idx);
    case prev :: current :: Nil => if (current >= prev) List(idx + 1) else Nil;
    case prev :: current :: next :: tail => {
      (if (current >= prev && current >= next) List(idx + 1) else Nil) :::
        __peaks(current :: next :: tail, idx + 1)
    };
  }
  {
    case current :: next :: tail => {
      (if (current >= next) List(0) else Nil) :::
        __peaks(current :: next :: tail, 1)
    };
    case default => __peaks(default, 0)
  }
}
peaks(1 :: 2 :: 3 :: 4 :: 5 :: Nil) // -> List(0,0,0,0,1) -> List(5)
peaks(5 :: 4 :: 3 :: 2 :: 1 :: Nil) // -> List(1,0,0,0,0) -> List(0)
peaks(1 :: 2 :: 5 :: 4 :: 3 :: Nil) // -> List(0,0,1,0,0) -> List(3)
peaks(1 :: 0 :: 1 :: 0 :: 1 :: Nil) // -> List(1,0,1,0,1) -> List(0,3,5)
peaks(Nil) // -> Nil
peaks(List(1)) // -> List(1) -> List(0)

```

# Вывод
Ознакомился с программированием на языке Scala на основе чистых функций.