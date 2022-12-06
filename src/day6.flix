namespace Day6 {

   def task(line: String, count: Int32): Int32 = {

    def go(ix: Int32, chars : List[Char]): Int32 = {
        if (List.length(chars) < count) -1
        else if (chars |> List.take(count) |> List.toSet |> Set.size == count) ix + count else go(ix + 1, List.drop(1, chars))
    };

    go(0, String.toList(line))
   }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input6.txt") {
            case Err(err) => println(err)
            case Ok(line::_) => {
                println(task(line, 4));
                println(task(line, 14));
                ()
            }
            case _ => println("empty file")
        };
        ()
    }
}


