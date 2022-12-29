"""
Generate a Mochi deck CSV for the periodic table.
"""
import csv
from io import StringIO

CSV = """1	Hydrogen	H
2	Helium	He
3	Lithium	Li
4	Beryllium	Be
5	Boron	B
6	Carbon	C
7	Nitrogen	N
8	Oxygen	O
9	Fluorine	F
10	Neon	Ne
11	Sodium	Na
12	Magnesium	Mg
13	Aluminum	Al
14	Silicon	Si
15	Phosphorus	P
16	Sulfur	S
17	Chlorine	Cl
18	Argon	Ar
19	Potassium	K
20	Calcium	Ca
21	Scandium	Sc
22	Titanium	Ti
23	Vanadium	V
24	Chromium	Cr
25	Manganese	Mn
26	Iron	Fe
27	Cobalt	Co
28	Nickel	Ni
29	Copper	Cu
30	Zinc	Zn
31	Gallium	Ga
32	Germanium	Ge
33	Arsenic	As
46	Palladium	Pd
47	Silver	Ag
49	Indium	In
50	Tin	Sn
53	Iodine	I
54	Xenon	Xe
60	Neodymium	Nd
74	Tungsten	W
77	Iridium	Ir
78	Platinum	Pt
79	Gold	Au
80	Mercury	Hg
82	Lead	Pb
84	Polonium	Po
86	Radon	Rn
88	Radium	Ra
90	Thorium	Th
92	Uranium	U
94	Plutonium	Pu"""

reader = csv.reader(StringIO(CSV), delimiter="\t")
print("Front,Back")
for row in reader:
    z: int = int(row[0])
    name: str = row[1]
    sym: str = row[2]
    print(f"What is the **symbol** of {name}?,{sym}")
    print(f"What is the **name** of the element with symbol {sym}?,{name}")
    print(f"What is the **atomic number** of {name}?,{z}")
    print(f"What is the **name** of the element with atomic number {z}?,{name}")
