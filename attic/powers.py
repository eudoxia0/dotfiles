def print_card(front, back):
    print(f"\"{front}\",\"{back}\"")

limit = 17

print("Front,Back")
for n in range(2, limit):
    p: int = 2**n
    print_card(
        f"Evaluate:\n\n$$2^{{{n}}}$$",
        f"$${p}$$",
    )
    print_card(
        f"Evaluate:\n\n$$\\log_2 {p}$$",
        f"$${n}$$",
    )

print()

for n in range(2, limit):
    p: int = 2**n
    print(f"1. $2^{{{n}}} = $ {{{{${p}$}}}}")
