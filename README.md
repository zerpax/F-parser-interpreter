| ФИО | Роль |
|-------------|-------------|
| Федоров Никита Сергеевич | Разработчик |
| Слесарчук Василий Анатольевич | Разработчик |

# Документация
## Основные синтаксические конструкции
### 1. Переменные
Переменные объявляются с помощью ключевого слова var и могут быть связаны с различными типами данных, включая числа, булевы значения, функции и списки.
```
var x = 42
```

### 2. Функции
Функции определяются с помощью ключевого слова fun. Можно определить как простую функцию, так и рекурсивную.

  Простая функция:
  ```
  fun add(a b) {
      a + b
  }
  ```
Рекурсивная функция:
```
rec fun factorial(n) {
    if n = 1 then {1} else {n * factorial(n - 1)}
}
```
### 3. Условные выражения
Для выполнения логических проверок и ветвления используются конструкции if, then и else.

```
var x = 6
if x > 10 then {
    print(1)
} else {
    print(0)
}
```
### 4. Применение функций
Применение функций к аргументам осуществляется с помощью круглых скобок.

```
add(2 3)
```
### 5. Списки
Списки создаются с помощью квадратных скобок и могут содержать элементы различных типов.
```
var numbers = [1 2 3 4 5]
```

## Типы данных
- Int — целые числа
- Float — числа с плавающей запятой
- Bool — булевы значения (true и false)
- List — списки, содержащие элементы любого поддерживаемого типа
## Встроенные операции
- Логические операции включают в себя стандартные операции, такие как &, |, !, =, !=, >, <, >=, <=.
- Поддерживаются следующие арифметическии операции: +, -, *, /.
