fun sum [] = 0
  | sum (x::L) = x + (sum L);


fun mult [] = 1
  | mult(x::L) = x * (mult L);


fun Mult [] = 1
  | Mult(r::R) = mult(r) * Mult(R);


fun mult' ([],a) = a
  | mult' (x::L, a) = mult' (L, x * a);


fun Mult' ([], a) = a
  | Mult' (r::R, a) = Mult'(R, mult'(r,a)); 



fun double (0 : int) : int = 0
  | double n = 2 + double (n - 1);

fun square (0:int) : int = 0
  | square n = square(n-1) + double(n - 1) + 1; 



fun divThree (0 : int) : bool = true
  | divThree(1 : int) : bool = false
  | divThree(2 : int) : bool = false
  | divThree (n) = if(n < 0) then divThree(~n) else divThree(n - 3); 


fun evenP(0 : int) : bool = false
  | evenP 1 = true
  | evenP n = evenP(n - 2);
