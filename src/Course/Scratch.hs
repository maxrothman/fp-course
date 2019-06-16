func1 :: (a, Either b c) -> Either (a, b) (a, c)
func1 (a, Left  b) = Left  (a, b)
func1 (a, Right c) = Right (a, c)

func2 :: Either (a, b) (a, c) -> (a, Either b c)
func2 (Left (a, b)) = (a, Left b)
func2 (Right (a, c)) = (a, Right c)