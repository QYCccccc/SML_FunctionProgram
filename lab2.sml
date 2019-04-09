(* 利用连接操作，将表头元素追加到表尾 *)
fun reverse ([]:int list):int list = [] 
  | reverse (x::L) = reverse(L) @ [x];
(* help函数 不断地将一个表的元素移动到另一个表 *)
fun revAppend ([], Ly) = Ly 
  | revAppend (x::Lx, Ly) = revAppend (Lx, x::Ly);

fun reverse' L = revAppend(L, []);

(* 依次从俩个列表中取出首元素 *)
fun interleave ([], Ly) = Ly
  | interleave (Lx, []) = Lx
  | interleave (x::Lx, y::Ly) = x::y:: interleave(Lx, Ly); 

(* 二叉树的结构 *)
datatype tree = Empty | Node of int * tree * tree;
(* 将第一个元素作为根结点,将列表分为两半 *)
fun listToTree [] = Empty
  | listToTree(x::xs) = 
      let
        val k = length xs div 2 
      in
        Node(x, listToTree(List.take(xs,k)), listToTree(List.drop(xs, k)))
      end;

fun trav Empty = []
  | trav (Node(x, t1, t2)) = trav t1 @ [x] @ trav t2;

fun revT Empty = Empty
  | revT(Node(x, t1, t2)) = Node(x, revT(t2), revT(t1));


fun binarySearch (Empty:tree, n:int) = false
  | binarySearch  (Node(x, t1, t2), n:int) = 
    case Int.compare(n, x) of
       GREATER => binarySearch(t2, n)
     | EQUAL => true
     | LESS => binarySearch(t1, n)