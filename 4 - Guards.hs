--solves a quadratic equation
quadroots :: Float -> Float -> Float -> String
quadroots a b c 
    | a == 0            = error "not quadratic"
    | discriminant == 0    = " one root " ++ show centre
    | otherwise         = " upper root " ++ show (centre + offset)
                        ++ " lower root " ++ show (centre - offset)
    where discriminant = b*b-4*a*c
          centre = -b/2*a
          offset = sqrt(discriminant/2*a)