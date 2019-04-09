fun square x = x * x;

fun thenAddOne (f, x) = f(x) + 1;

val t1 = thenAddOne(square, 3);

fun mapList(f, []) = []
    | mapList(f, x::L) = f(x)::mapList(f, L);

val t2 = mapList(square, [1,2,3,4,5]);

fun mapList' f = 
    let
        fun f2 [] = []
            | f2(x::L) = f(x)::f2(L)
    in 
        f2
    end

val t3 = mapList' square;
val t4 = t3 [1,2,3,4,5];

fun findOdd [] = NONE
    | findOdd(x::L) = 
    if ((x mod 2) = 1)
        then SOME x
    else
        findOdd L;

val t5 = findOdd([2,3,4,5,6,7,8,9]);

(* 编写函数subsetSumOption: int list * int -> int list option，要求：
对函数subsetSumOption(L, s)：如果L中存在子集L’，满足其中所有元素之和为s，则结果为SOME L’；否则结果为NONE。 *)
fun map f [] = []
    | map f(x::R) = (f x)::(map f R)

fun sublists [] = [[]]
    | sublists (x::R) = 
        let
          val S = sublists R
        in
          S @ map (fn A => x::A) S
        end

fun sum [] = 0
    | sum(x::L) = x + sum(L)

fun findsubsetSum ([[]], s) = NONE
    | findsubsetSum (L::R, s) = 
        if (sum(L)=s)
            then SOME L
        else
            findsubsetSum(R, s) 
fun subsetSumOption ([], s) = NONE
    | subsetSumOption (L, s) = findsubsetSum(sublists L, s)

val t6 = subsetSumOption([1,2,3,4,5,6], 8);

