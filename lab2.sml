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


fun split L = 
fun listToTree