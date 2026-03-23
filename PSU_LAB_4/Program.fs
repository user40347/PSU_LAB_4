open System

//Структура ветви
type Tree<'a> = 
    | Node of value:'a * children: Tree<'a> list

let rec map f (Node (v, children)) =
    Node(f v, List.map (map f) children)

    // Обработка вводимого положительного целого числа
let rec readPositiveInt (prompt: string) =
    printf "%s" prompt
    match System.Console.ReadLine () with
    | null | "" ->
        printfn "Ошибка: введите целое число."
        readPositiveInt prompt
    | s ->
        match System.Int32.TryParse s with
        | true, v when v >= 0 -> v
        | _ ->
            printfn "Ошибка: введите число > 0."
            readPositiveInt prompt

//Построение дерева
let rec readFromConsole () : Tree<string> =
    printf "Введите значение узла: "
    let value = Console.ReadLine ()
    let count = readPositiveInt "Сколько потомков у этого узла? "
    let children =
        [ for _ in 1 .. count do
            yield readFromConsole () ]
    Node (value, children)

//Вывод структуры дерева
let rec printTree indent (Node (v, children)) =
    printfn  "%s\%s" (String.replicate (indent) "  ") v
    children |> List.iter (fun child -> 
        printTree (indent + 2) child)

// Ввод символа
let rec readChar () =
    printf 
        "Введите символ, для добавления в конец каждой строки:"
    let symbolLine = Console.ReadLine ()
    if not (String.IsNullOrEmpty symbolLine) then 
        symbolLine.[0]
    else
        printfn "Пустая строка — повторите ввод."
        readChar ()

[<EntryPoint>]
let main argv =
    printfn "Построим исходное дерево."
    let tree = readFromConsole ()
    let char = readChar ()
    let newTree = map (fun s -> s + string char) tree
    printfn "\nИсходное дерево :"
    printTree 0 tree
    printfn "\nНовое дерево (после добавления '%c'):" char
    printTree 0 newTree
    0