
-- Лабораторна 4: типи і класи типів
-- Варіант 1

------- Завдання 1 -------

-- обчислення площі фігури (для мітки через розмір одного символу та їх кількість)

data Font = Font { name :: String, size :: Float} deriving (Show)
data Point = Point Float Float deriving (Show)


data Shape = Label {start_point :: Point, font :: Font, count_in_row :: Int} 
            | Rectangle Point Point
            | Triangle Point Point Point
            | Circle Point Float deriving (Show) 



area :: Shape -> Float
area (Label _ (Font _ size) count_in_row) = size * fromIntegral count_in_row
area (Circle _ r) = pi * r ^ 2  
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)  
area (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = 
    sqrt (s*(s-a)*(s-b)*(s-c))
    where
        a = distance (Point x1 y1) (Point x2 y2)
        b = distance (Point x2 y2) (Point x3 y3)
        c = distance (Point x3 y3) (Point x1 y1)
        s = (a + b + c) / 2


distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) =
    sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2

-- Usage example: 
-- : load FP_lab_work_4_variant_1
-- let lab = Label {start_point=(Point 0 0), font=Font {name="Whatever", size=10}, count_in_row=7}
-- area lab
-- Triangle>> area (Triangle (Point 0 0) (Point 3 3) (Point 5 0))
