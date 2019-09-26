data LogicExpr = V String
                 | Negation  LogicExpr
                 | Conjunction LogicExpr LogicExpr
                 | Disjunction  LogicExpr LogicExpr 
                 | Implication  LogicExpr LogicExpr 

alpha = Conjunction (Implication (V "p1") (Conjunction (V "p2") (V "p3"))) 
					(Implication (Negation (V "p1")) (Conjunction (V "p3") (V "p4")))

beta = Conjunction (Implication (V "p3") (Negation (V "p6"))) 
				   (Implication (Negation (V "p3")) (Implication (V "p4") (V "p1"))) 

gamma = Conjunction (Negation (Conjunction (V "p2") (V "p5")))		
					(Implication (V "p2") (V "p5"))

delta = Negation (Implication (V "p3") (V "p6"))

psi = Implication (Conjunction alpha (Conjunction beta gamma)) (delta)

i1 = [("p1", False), ("p2", True), ("p3", False), ("p4", True), ("p5", False), ("p6", True)]

i2 = [("p1", True), ("p2", False), ("p3", True), ("p4", False), ("p5", True), ("p6", False)]

evaluate :: LogicExpr -> [(String,Bool)] -> Bool

evaluate (V a) ((x1,x2):xs) | a==x1 = x2
                            | otherwise = (evaluate(V a)xs)

evaluate (Negation a) l | (evaluate a l)==True = False
                        | otherwise = True

evaluate (Conjunction a b) l = (evaluate a l)&&(evaluate b l)

evaluate (Disjunction a b) l = (evaluate a l)||(evaluate b l)

evaluate (Implication a b) l
    | (((evaluate b l)==False)&&((evaluate a l)==True)) = False
    | otherwise = True