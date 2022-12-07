namespace Day7 {

    enum Tree {
        case File(String, Int32)
        case Dir(String, Option[List[Tree]])
    }

    def parse(tree: Tree, cwd: List[String], lines: List[String]): Result[Tree, String] = {
        match lines {
            case ("$ cd /" :: rest) => parse(tree, Nil, rest)
            case ("$ cd .." :: rest) => parse(tree, Option.getWithDefault(Nil, List.init(cwd)), rest)
            case ("$ ls" :: rest) => {
                use Result.flatMap;
                let* items = rest |> List.takeWhile(l -> not String.startsWith({prefix="$ "}, l)) |> Result.traverse(l -> {
                    if (String.startsWith({prefix="dir "}, l)) Ok(Dir(String.drop(4, l), None)) else match String.splitOn({substr =" "}, l ) {
                        case size :: name :: Nil => Result.map(s -> File(name, s), Int32.parse(10, size))
                        case _ => Err("Expected ls output to be '<filesize> <filename>', but got: ${l}")
                    }
                });
                updateTree(tree, cwd, items)
                    |> Result.flatMap(newTree -> parse(newTree, cwd, List.drop(List.length(items), rest)))
            }
            case (c :: rest) => if (String.startsWith({prefix = "$ cd "}, c)) parse(tree, List.append(cwd, String.drop(5, c) :: Nil), rest) else Err("Unkown command ${c}")
            case Nil => Ok(tree)
        }
    }

    def updateTree(tree: Tree, dirs: List[String], items: List[Tree]): Result[Tree, String] = {
        match (tree, dirs) {
            case (Dir(name, _), Nil) => Ok(Dir(name, Some(items)))
            case (Dir(name, Some(contents)), d :: ds) => {
                Result.traverse(i -> match i {
                    case File(_, _) => Ok(i)
                    case Dir(dn, _) => if (d == dn) updateTree(i, ds, items) else Ok(i)
                }, contents)
                |> Result.map(nc -> Dir(name, Some(nc)))
            }
            case _ => Err("Error")
        }
    }

    def printTree(prefix: String, tree: Tree): Unit \ IO = {
        match tree {
            case File(name, size) => println("${prefix}- ${name} (file, size = ${size})")
            case Dir(name, contents) => {
                println("${prefix}- ${name} (dir)");
                Option.forEach(list -> List.forEach(i -> printTree(prefix ++ "  ", i), list), contents)
            }
        }
    }

    enum DirSize(String, Int32, List[DirSize])

    def calcDirSize(tree: Tree): Option[DirSize] = {
        match tree {
            case File(_, _) => None
            case Dir(name, None) => Some(DirSize(name, 0, Nil))
            case Dir(name, Some(contents)) => {
                let sumFiles = List.sum(List.map(f -> match f {
                    case File(_, size) => size,
                    case _ => 0
                }, contents));
                let dirSizes = List.filterMap(calcDirSize, contents);
                let sumDirSizes = List.sum(List.map(match DirSize(_, size, _) -> size, dirSizes));
                Some(DirSize(name, sumFiles  + sumDirSizes, dirSizes))
            }
        }
    }

    def flattenDirSizes(d: DirSize): List[DirSize] = match d {
        case DirSize(_name, _size, contents) => d :: List.flatMap(flattenDirSizes, contents)
    }

   def task1(lines: List[String]): Option[Int32] \ IO  = {
        let parsed = parse((Dir("/", None)) ,Nil, lines);
        use Option.flatMap;
        let* tree = Result.toOption(parsed);
        printTree("", tree);
        let* treeSize =  calcDirSize(tree);
         treeSize
            |> flattenDirSizes
            |> List.map(match DirSize(_name, size, _contents) -> size)
            |> List.filter(s -> s <= 100000)
            |> List.sum
            |> Some
   }

   def task2(lines: List[String]): Option[Int32]  = {
        let parsed = parse((Dir("/", None)) ,Nil, lines);
        use Option.flatMap;
        let* tree = Result.toOption(parsed);
        let* ds = calcDirSize(tree);
        let list = flattenDirSizes(ds);
        let* DirSize(_, rootDirSize, _) = List.head(list);
        let minDeleteSize = 30000000 - 70000000 + rootDirSize;
        list |> List.map(match DirSize(_name, size, _contents) -> size)
            |> List.filter(s -> s > minDeleteSize)
            |> List.minimum
   }

    pub def main(): Unit \ IO = {
        match File.readLines("src/input7.txt") {
            case Err(err) => println(err)
            case Ok(lines) => {
                println(task1(lines));
                println(task2(lines))
            }
        }
    }
}
