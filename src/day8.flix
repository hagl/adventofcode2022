namespace Day8 {


   def task1(input: List[List[Char]]): Int32 \ IO = {
        let area = List.mapWithIndex(y -> List.mapWithIndex(x -> h -> ((x,y), (Char.toInt32(h)-48))), input);
        let visibles : Set[(Int32, Int32)] = Set.empty();
        let visiblesLR1 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), row))
        , visibles, area);
        println(visiblesLR1);
        let visiblesRL2 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), List.reverse(row)))
        , visiblesLR1, area);
        println(visiblesRL2);
        let visiblesTD3 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), row))
        , visiblesRL2, List.transpose(area));
        println(visiblesTD3);
        let visiblesDT4 = List.foldLeft(v -> row ->
            fst(List.foldLeft(match (vr, currentHeight) -> match (xy, h) -> if (h <= currentHeight) (vr, currentHeight) else (Set.insert(xy, vr), h), (v, -1), List.reverse(row)))
        , visiblesTD3, area);
        println(visiblesDT4);
        Set.size(visiblesDT4)
   }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input8ex.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(List.map(String.toList, lines)));
                ()
            }
        };
        ()
    }
}


