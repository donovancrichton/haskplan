

(define (problem BW-rand-10)
(:domain blocksworld)
(:objects b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 )
(:init
(on b1 b7)
(on b2 b9)
(on b3 b6)
(on b4 b3)
(on b5 b10)
(on-table b6)
(on b7 b5)
(on b8 b4)
(on b9 b8)
(on b10 b2)
(clear b1)
)
(:goal
(and
(on b2 b4)
(on b4 b7)
(on b6 b1)
(on b7 b6)
(on b8 b2))
)
)


