module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer name =
    let
        person = Maybe.withDefault "you" name
    in 
        "One for " ++ person ++ ", one for me."
    
