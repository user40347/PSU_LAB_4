open System

// Структура ветви
type Tree<'a> = 
    | Node of value:'a * children: Tree<'a> list

let rec map f (Node (v, children)) =
    Node(f v, List.map (map f) children)

// Проверка вводимого целого числа
let rec readInt (prompt: string) =
    printf "%s" prompt
    match System.Console.ReadLine () with
    | null | "" ->
        printfn "Ошибка: введите целое число."
        readInt prompt
    | s ->
        match System.Int32.TryParse s with
        | true, v -> v
        | _ ->
            printfn "Ошибка: некорректный формат целого числа."
            readInt prompt

// Построение дерева целых чисел
let rec readFromConsole () : Tree<int> =
    let value = readInt "Введите значение узла (число): "
    let count = readInt $"Сколько потомков у узла {value}? "
    let children =
        [ for _ in 1 .. count do
            yield readFromConsole () ]
    Node (value, children)

// Вывод структуры дерева
let rec printTree indent (Node (v, children)) =
    printfn  "%s\%dx" (String.replicate (indent) "  ") v
    children |> List.iter (fun child -> printTree (indent + 2) child)

// Функция fold для обхода дерева
let rec foldTree f acc (Node (v, children)) =
    let newAcc = f acc v
    List.fold (fun acc child -> foldTree f acc child) newAcc children

// Функция для сбора четных чисел
let collectEvenNumbers tree =
    foldTree (fun acc x -> if x % 2 = 0 then x :: acc else acc) [] tree

[<EntryPoint>]
let main argv =
    printfn "Построим исходное дерево."
    let tree = readFromConsole ()
    printfn "\nИсходное дерево:"
    printTree 0 tree

    // Собираем четные числа
    let evenNumbers = collectEvenNumbers tree
    printfn "\nЧетные числа дерева: %A" evenNumbers

    0