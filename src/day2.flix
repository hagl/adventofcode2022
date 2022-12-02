namespace Day2 {

    def task1(lines: List[String]): Int32 = {
        let scores = List.map(l -> match l {
            case "A X" => 1 + 3
            case "A Y" => 2 + 6
            case "A Z" => 3 + 0
            case "B X" => 1
            case "B Y" => 2 + 3
            case "B Z" => 3 + 6
            case "C X" => 1 + 6
            case "C Y" => 2
            case "C Z" => 3 + 3
            case _ => 0
        }, lines);
        List.sum(scores)
    }

    def task2(lines: List[String]): Int32 = {
        let scores = List.map(l -> match l {
            case "A X" => 3 + 0
            case "A Y" => 1 + 3
            case "A Z" => 2 + 6
            case "B X" => 1
            case "B Y" => 2 + 3
            case "B Z" => 3 + 6
            case "C X" => 2
            case "C Y" => 3 + 3
            case "C Z" => 1 + 6
            case _ => 0
        }, lines);
        List.sum(scores)
    }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input2.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(lines));
                println(task2(lines))
            }
        };
        ()
    }
}


