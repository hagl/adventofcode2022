namespace Day3 {

    def score(c: Char): Int32 = {
        let i = Char.toInt32(c);
        if (Char.isUpperCase(c)) {
            i - Char.toInt32('A') + 27
        } else {
            i - Char.toInt32('a') + 1
        }
    }

    def task1(lines: List[String]): Int32 = {
        let scores = List.map(l -> {
            let (c1, c2) = List.splitAt(String.length(l) / 2, String.toList(l));
            Set.intersection(List.toSet(c1), List.toSet(c2))
              |> Set.map(score)
              |> Set.sum
        }, lines);
        List.sum(scores)
    }

   def task2(lines: List[String]): Int32 = {
    let groups = List.unfold(l -> match l {
        case (a :: b :: c :: r) => Some(((a,b,c), r))
        case _ => None
    }, lines);
    // let groups = List.zip3(lines, List.drop(1, lines), List.drop(2, lines));
    let scores : List[Int32]= List.map( x -> {
        let (a,b,c) = x;
        Set.intersection(Set.intersection(List.toSet(String.toList(a)), List.toSet(String.toList(b))), List.toSet(String.toList(c)))
            |> Set.map(score)
            |> Set.sum
   }, groups);
    List.sum(scores)
   }
//    def task2(lines: List[String]): Int32 = {
//         let groups : List[(String, String, String)] = List.zip3(lines, List.drop(1, lines), List.drop(2, lines));
//         let scores = List.map((a,b,c) -> {
//             Set.intersection(Set.intersection(List.toSet(a), List.toSet(b)), List.toSet(c))
//               |> Set.map(score)
//               |> Set.sum
//         }, groups);
//         List.sum(scores)
//     }


    pub def main(): Unit \ IO = {
        match File.readLines("src/input3.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(lines));
                println(task2(lines))
            }
        };
        ()
    }
}


