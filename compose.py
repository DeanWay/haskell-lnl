def is_big(x: int) -> bool:
    return x > 9000

def bool_to_yes_no(x: bool) -> str:
    return "Yes" if x else "No"

def my_program(x: int) -> str:
    return bool_to_yes_no(is_big(x))

if __name__ == "__main__":
    line = input("Is it a big number? ")
    print(my_program(int(line)))
