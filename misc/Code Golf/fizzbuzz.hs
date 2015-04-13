main=mapM_(\a->putStrLn$[show a,"Buzz","Fizz","FizzBuzz"]!!(0^mod a 5+2*0^mod a 3))[1..100]
