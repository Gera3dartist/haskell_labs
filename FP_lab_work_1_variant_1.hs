
-- Лабораторна 1
-- Варіант 1

------- Завдання 1 -------


-- 2.1 Функцiя приймає три числа i перевiряє, чи значення першого з них
-- знаходиться мiж значеннями двох iнших.


inrange :: (Integer, Integer, Integer) -> Bool; inrange (x, y, z) = min z y  < x &&  max z y > x
inrange2 :: Integer -> Integer -> Integer -> Bool; inrange2 x y z = min z y  < x &&  max z y > x

