

(define (problem BW-rand-20)
(:domain blocksworld)
(:objects b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 )
(:init
(on b1 b7)
(on b2 b12)
(on b3 b6)
(on-table b4)
(on b5 b17)
(on b6 b2)
(on b7 b15)
(on b8 b10)
(on b9 b20)
(on b10 b5)
(on b11 b18)
(on-table b12)
(on b13 b8)
(on b14 b9)
(on-table b15)
(on b16 b3)
(on b17 b1)
(on b18 b13)
(on b19 b11)
(on b20 b4)
(clear b14)
(clear b16)
(clear b19)
)
(:goal
(and
(on b1 b18)
(on b2 b7)
(on b3 b20)
(on b4 b8)
(on b5 b19)
(on b7 b14)
(on b8 b9)
(on b9 b3)
(on b10 b4)
(on b11 b6)
(on b12 b16)
(on b13 b15)
(on b16 b10)
(on b17 b1)
(on b18 b12)
(on b20 b13))
)
)


