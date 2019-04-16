(* 编写函数thenAddOne，要求：
函数类型为: ((int ->int) * int) -> int；
功能为将一个整数通过函数变换(如翻倍、求平方或求阶乘)后再加1。 *)

fun square x = x * x;

fun thenAddOne (f, x) = f(x) + 1;

val t1 = thenAddOne((fn x => x * x), 3);


(* 编写函数mapList，要求：
函数类型为: ((‘a -> ‘b) * ‘a list) -> ‘b list；
功能为实现整数集的数学变换(如翻倍、求平方或求阶乘) *)

fun mapList(f, []) = []
    | mapList(f, x::L) = f(x)::mapList(f, L);

val t2 = mapList(square, [1,2,3,4,5]);

(* 编写函数mapList’，要求：① 函数类型为: (‘a -> ‘b) -> (‘a list -> ‘b list)；   
② 功能为实现整数集的数学变换(如翻倍、求平方或求阶乘)。
③ 比较函数mapList’和mapList，分析、体会它们有什么不同。 *)

fun mapList' f = 
    let
        fun f2 [] = []
            | f2(x::L) = f(x)::f2(L)
    in 
        f2
    end

val t3 = mapList' square;
val t4 = t3 [1,2,3,4,5];

(* 4.编写函数findOdd，要求：① 函数类型为: int list -> int option；   
② 功能为：如果x为L中的第一个奇数，则返回SOME x；否则返回NONE *)
fun findOdd [] = NONE
    | findOdd(x::L) = 
    if ((x mod 2) = 1)
        then SOME x
    else
        findOdd L;

val t5 = findOdd([2,3,4,5,6,7,8,9]);

(* 编写函数subsetSumOption: int list * int -> int list option，要求：
对函数subsetSumOption(L, s)：如果L中存在子集L’，满足其中所有元素之和为s，则结果为SOME L’；否则结果为NONE。 *)


fun subsetSumoption([],s) = NONE
    |subsetSumoption(a::l,s) = if s=a then SOME (a::[]) else case subsetSumoption(l,s-a) of
       NONE => subsetSumoption(l,s)
     | SOME x => SOME (a::x)

val tx = subsetSumoption([1,2,3,4,5,6], 8);

(* 编写函数：
	exists: (‘a -> bool) -> ‘a list -> bool
	forall: (‘a -> bool) -> ‘a list -> bool
   对函数p: t -> bool, 整数集L: t list,
	有：exist p L =>* true if there is an x in L such that p x=true;
	         exits p L =>* false otherwise.
	         forall p L =>* true if p x = true for every item x in L;
	         forall p L =>* false otherwise. *)

fun p x = 
    if (x > 4)
        then false
    else 
        true


fun exists (f) = 
    let
      fun f2 [] = false
        | f2 (x::L) = 
            if (f(x))
                then true
            else
                f2 L 
    in
      f2
    end

val t7 = exists p [1,2,3,4,5];
val t8 = exists p [5,6,7,8];

fun forall (f) = 
    let
       fun f2 [] = true
        | f2(x::L) = 
            if (f(x))
                then f2 L
            else 
                false
    in
      f2
    end

val t9 = forall p [1,2,3,4];
val t10 = forall p [1,2,3,4,5];

(* 编写函数：
	treeFilter: (‘a -> bool) -> ‘a tree -> ‘a option tree
    将树中满足条件P（ ‘a -> bool ）的节点封装成option类型保留，否则替换成NONE。 *)

datatype 'a tree = Lf
    | Br of 'a * 'a tree * 'a tree


fun treeFilter (f, T) = case T of 
    Lf => Lf
    | Br(x, t1, t2) => case f(x) of
        true => Br(SOME x, treeFilter(f, t1), treeFilter(f, t2)) 
        | _ => Br(NONE, treeFilter(f, t1), treeFilter(f, t2))

val ta = treeFilter((fn x => case x of
   1 => true
 | _ => false), Br(1, Br(2, Lf, Lf), Br(3, Lf, Lf)));
