namespace Day4 {

    //24-66,23-25
    //3-3,2-80
    def parse(line: String): Option[((Int32,Int32), (Int32,Int32))] = {
        match List.map(Int32.parse(10), String.split({regex="[,-]"}, line)) {
            case Ok(f1) :: Ok(t1) :: Ok(f2) :: Ok(t2) :: Nil => Some (((f1, t1), (f2, t2)))
            case _ => None
        }
    }

    def task2(lines: List[String]): Int32 = {
        List.filterMap(parse, lines)
        |> List.filter(e -> {
            let ((f1,t1), (f2,t2)) = e;
            (f1 >= f2 and f1 <= t2) or (t1 >= f2 and t1 <= t2)
            or (f2 >= f1 and f2 <= t1) or (t2 >= f1 and t2 <= t1)
        })
        |> List.length
    }

   def task1(lines: List[String]): Int32 = {
        List.filterMap(parse, lines)
        |> List.filter(e -> {
            let ((f1,t1), (f2,t2)) = e;
            (f1 >= f2 and t1 <= t2) or (f2 >= f1 and t2 <= t1)
        })
        |> List.length
    }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input4.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(lines));
                println(task2(lines))
            }
        };
        ()
    }
}


