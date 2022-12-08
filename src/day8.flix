namespace Day8 {


   def task1(input: List[List[Char]]): Int32 \ IO = {
        let area = List.mapWithIndex(y -> List.mapWithIndex(x -> h -> ((x,y), (Char.toInt32(h)-48))), input);
        let visibles : Set[(Int32, Int32)] = Set.empty();
        let visiblesLR1 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), row))
        , visibles, area);
        println(visiblesLR1);
        printGrid(5, 5, visiblesLR1);
        let visiblesRL2 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), List.reverse(row)))
        , visiblesLR1, area);
        println(visiblesRL2);
        printGrid(5, 5, visiblesRL2);
        let visiblesTD3 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), row))
        , visiblesRL2, List.transpose(area));
        println(visiblesTD3);
        printGrid(5, 5, visiblesTD3);
        let visiblesDT4 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), List.reverse(row)))
        , visiblesTD3, List.transpose(area));
        println(visiblesDT4);
        printGrid(5, 5, visiblesDT4);
        Set.size(visiblesDT4)
   }

    def printGrid(xMax: Int32, yMax: Int32, set: Set[(Int32, Int32)]): Unit \ IO = {
        List.forEach(y -> {
            List.forEach(x -> if (Set.memberOf((x,y), set)) print('X') else print("_"), List.range(0, xMax));
            println("")
        }, List.range(0, yMax))
    }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input8.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(List.map(String.toList, lines)));
                ()
            }
        };
        ()
    }
}


