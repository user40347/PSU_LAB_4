open System

// Структура ветви
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
            printfn "Ошибка: введите число >= 0."
            readPositiveInt prompt

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

// Выбор метода заполнения
let rec readSelectedMethod () =
    printfn "Выберите способ заполнения:
    1) С клавиатуры
    2) Случайными числами"
    
    match Console.ReadLine() with
    | "1" | "2" as method -> method
    | _ ->
        printfn "Ошибка: выберите 1 или 2"
        readSelectedMethod()

// Построение дерева
let rec readFromConsole (selectedMethod: string) : Tree<int> =
    let (value, count) = 
        if selectedMethod = "1" then
            let v = readInt "Введите значение узла: "
            let c = readPositiveInt "Сколько потомков у этого узла? "
            (v, c)
        else
            let rnd = Random()
            let v = rnd.Next(0, 10)
            let c = rnd.Next(0, 3)  // 0, 1 или 2 потомка
            (v, c)
    
    let children = 
        [ for _ in 1..count -> readFromConsole selectedMethod ]
    
    Node(value, children)

// Вывод структуры дерева
let rec printTree indent (Node (v, children)) =
    printfn "%s%d" (String.replicate indent "  ") v
    children |> List.iter (printTree (indent + 2))

// Функция fold для обхода дерева
let rec foldTree f acc (Node (v, children)) =
    let newAcc = f acc v
    List.fold (fun acc child -> foldTree f acc child) newAcc children

// Функция для сбора четных чисел
let collectEvenNumbers tree =
    foldTree (fun acc x -> if x % 2 = 0 then x :: acc else acc) [] tree
    |> List.rev  // Чтобы сохранить порядок обхода

[<EntryPoint>]
let main argv =
    printfn "Построим исходное дерево."
    let selectedMethod = readSelectedMethod()  // Добавлено получение метода
    let tree = readFromConsole selectedMethod  // Исправлен вызов с аргументом
    printfn "\nИсходное дерево:"
    printTree 0 tree
    
    // Собираем четные числа
    let evenNumbers = collectEvenNumbers tree
    printfn "\nЧетные числа дерева: %A" evenNumbers

    0  // Корректный возврат кода завершения