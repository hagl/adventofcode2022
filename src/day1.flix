def toLists(list: List[String]): List[List[Int32]] = {
    let (c, r) =
        List.foldLeft(
            match (c, r) -> {
                e ->
                match Int32.parse(10, e) {
                    case Err(_e) => (Nil, c :: r)
                    case Ok(n) => (n :: c, r)
                }},
            (Nil, Nil),
            list);
    c :: r
}

def task1(lines: List[String]): Option[Int32] = {
    let lists = toLists(lines);
    let cals = List.map(List.sum, lists);
    List.maximum(cals)
}


def task2(lines: List[String]): Int32 = {
    let lists = toLists(lines);
    let cals = List.map(List.sum, lists);
    let sorted = List.sortWith(a -> b -> b <=> a, cals);
    List.sum(List.take(3, sorted))
}
def day1(): Unit \ IO = {
    match File.readLines("src/input1.txt") {
        case Err(err) => println(err)
        case Ok(lines) => {
            println(task1(lines));
            println(task2(lines))
        }
    };
    ()
}
