open System

// Структура ветви
type Tree<'a> = 
    | Node of value:'a * children: Tree<'a> list

let rec map f (Node (v, children)) =
    Node (f v, List.map (map f) children)

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
            printfn "Ошибка: введите число >= 0."
            readPositiveInt prompt

// Выбор метода заполнения списка
let rec readSelectedMethod () =
    printfn "Выберите способ заполнения:
    1) С клавиатуры
    2) Путем заполнения случайными числами"

    match Console.ReadLine () with
    | "1" | "2" as method -> method
    | _ ->
        printfn "Такого метода нет. Попробуйте снова."
        readSelectedMethod ()

// Построение дерева
let rec readFromConsole (selectedMethod: string) : Tree<string> =
    let (value, count) = 
        if selectedMethod = "1" then
            printf "Введите значение узла: "
            let v = Console.ReadLine()
            let c = readPositiveInt "Сколько потомков у этого узла? "
            (v, c)
        else
            let rnd = Random()
            let v = rnd.Next(0, 10).ToString()
            let c = rnd.Next(0, 3)  // 0, 1 или 2 потомка
            (v, c)
    
    let children = 
        [ for _ in 1..count -> readFromConsole selectedMethod ]
    
    Node(value, children)

// Вывод структуры дерева
let rec printTree indent (Node (v, children)) =
    printfn "%s\\%s" (String.replicate indent "  ") v
    children |> List.iter (printTree (indent + 2))

// Ввод символа
let rec readChar () =
    printf "Введите символ, для добавления в конец каждой строки: "
    let symbolLine = Console.ReadLine ()
    if not (String.IsNullOrEmpty symbolLine) then 
        symbolLine.[0]
    else
        printfn "Пустая строка — повторите ввод."
        readChar ()

[<EntryPoint>]
let main argv =
    printfn "Построим исходное дерево."
    let selectedMethod = readSelectedMethod ()
    let tree = readFromConsole selectedMethod
    let char = readChar ()
    let newTree = map (fun s -> s + string char) tree
    printfn "\nИсходное дерево :"
    printTree 0 tree
    printfn "\nНовое дерево (после добавления '%c'):" char
    printTree 0 newTree
    0