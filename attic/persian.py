"""
# Overview

Generate a Mochi deck CSV for the Persian alphabet.
"""
from dataclasses import dataclass

@dataclass
class Letter:
    final: str
    medial: str
    initial: str
    isolated: str
    name: str
    pronunciation: str

ALPHABET: list[Letter] = [
    # Alef
    Letter(
        final="ـا",
        medial="ـا",
        initial="ا or آ",
        isolated="ا",
        name="Alef",
        pronunciation="- initial آ: as in c**augh**t\\n- initial ا: **a** in cat, **e** in bed, **o** in not\\n- other ا: **a** in cat",
    ),
    # Be
    Letter(
        final="ـب",
        medial="ـبـ",
        initial="بـ",
        isolated="ب",
        name="Be",
        pronunciation="**b** in bed",
    ),
    # Pe
    Letter(
        final="ـپ",
        medial="ـپـ",
        initial="پـ",
        isolated="پ",
        name="Pe",
        pronunciation="**p** in put",
    ),
    # Te
    Letter(
        final="ـت",
        medial="ـتـ",
        initial="تـ",
        isolated="ت",
        name="Te",
        pronunciation="**t** in ten",
    ),
    # Se
    Letter(
        final="ـث",
        medial="ـثـ",
        initial="ثـ",
        isolated="ث",
        name="Se",
        pronunciation="**s** in sum",
    ),
    # Jim
    Letter(
        final="ـج",
        medial="ـجـ",
        initial="جـ",
        isolated="ج",
        name="Jim",
        pronunciation="**j** in jump",
    ),
    # Che
    Letter(
        final="ـچ",
        medial="ـچـ",
        initial="چـ",
        isolated="چ",
        name="Che",
        pronunciation="**ch** in church",
    ),
    # He jimi
    Letter(
        final="ـح",
        medial="ـحـ",
        initial="حـ",
        isolated="ح",
        name="He jimi",
        pronunciation="**h** in hat",
    ),
    # Xe
    Letter(
        final="ـخ",
        medial="ـخـ",
        initial="خـ",
        isolated="خ",
        name="Xe",
        pronunciation="**ch** in loch",
    ),
    # Dal
    Letter(
        final="ـد",
        medial="ـد",
        initial="د",
        isolated="د",
        name="Dal",
        pronunciation="**d** in dye",
    ),
    # Zal
    Letter(
        final="ـذ",
        medial="ـذ",
        initial="ذ",
        isolated="ذ",
        name="Zal",
        pronunciation="**z** in zoo",
    ),
    # Re
    Letter(
        final="ـر",
        medial="ـر",
        initial="ر",
        isolated="ر",
        name="Re",
        pronunciation="**r** in rat",
    ),
    # Ze
    Letter(
        final="ـز",
        medial="ـز",
        initial="ز",
        isolated="ز",
        name="Ze",
        pronunciation="**z** in zoo",
    ),
    # Zhe
    Letter(
        final="ـژ",
        medial="ـژ",
        initial="ژ",
        isolated="ژ",
        name="Zhe",
        pronunciation="**si** in vision, or **s** in pleasure",
    ),
    # Sin
    Letter(
        final="ـس",
        medial="ـسـ",
        initial="سـ",
        isolated="س",
        name="Sin",
        pronunciation="**s** in sum",
    ),
    # Shin
    Letter(
        final="ـش",
        medial="ـشـ",
        initial="شـ",
        isolated="ش",
        name="Shin",
        pronunciation="**sh** in sheep",
    ),
    # Sad
    Letter(
        final="ـص",
        medial="ـصـ",
        initial="صـ",
        isolated="ص",
        name="Sad",
        pronunciation="**s** in sum",
    ),
    # Zad
    Letter(
        final="ـض",
        medial="ـضـ",
        initial="ضـ",
        isolated="ض",
        name="Zad",
        pronunciation="**z** in zoo",
    ),
    # Ta
    Letter(
        final="ـط",
        medial="ـطـ",
        initial="طـ",
        isolated="ط",
        name="Ta",
        pronunciation="**t** in cat",
    ),
    # Za
    Letter(
        final="ـظ",
        medial="ـظـ",
        initial="ظـ",
        isolated="ظ",
        name="Za",
        pronunciation="**z** in zoo",
    ),
    # Ayn
    Letter(
        final="ـع",
        medial="ـعـ",
        initial="عـ",
        isolated="ع",
        name="Ayn",
        pronunciation="Glottal stop.",
    ),
    # Gayn
    Letter(
        final="ـغ",
        medial="ـغـ",
        initial="غـ",
        isolated="غ",
        name="Gayn",
        pronunciation="Uvular r, like the **r** in the French pronunciation of 'Paris'",
    ),
    # Fe
    Letter(
        final="ـف",
        medial="ـفـ",
        initial="فـ",
        isolated="ف",
        name="Fe",
        pronunciation="**f** in fat",
    ),
    # Qaf
    Letter(
        final="ـق",
        medial="ـقـ",
        initial="قـ",
        isolated="ق",
        name="Qaf",
        pronunciation="**c** in cut",
    ),
    # Kaf
    Letter(
        final="ـک",
        medial="ـکـ",
        initial="کـ",
        isolated="ک",
        name="Kaf",
        pronunciation="**k** in sky",
    ),
    # Gaf
    Letter(
        final="ـگ",
        medial="ـگـ",
        initial="گـ",
        isolated="گ",
        name="Gaf",
        pronunciation="**g** in get",
    ),
    # Lam
    Letter(
        final="ـل",
        medial="ـلـ",
        initial="لـ",
        isolated="ل",
        name="Lam",
        pronunciation="**l** in let",
    ),
    # Mim
    Letter(
        final="ـم",
        medial="ـمـ",
        initial="مـ",
        isolated="م",
        name="Mim",
        pronunciation="**m** in him",
    ),
    # Nun
    Letter(
        final="ـن",
        medial="ـنـ",
        initial="نـ",
        isolated="ن",
        name="Nun",
        pronunciation="**n** in not",
    ),
    # Vav
    Letter(
        final="ـو",
        medial="ـو",
        initial="و",
        isolated="و",
        name="Vav",
        pronunciation="Multiple: **v** in very, or **u** in put, or **ow** in grow, or **o** in not (only at the end of a word).",
    ),
    # He docheshm
    Letter(
        final="ـه",
        medial="ـهـ",
        initial="هـ",
        isolated="ه",
        name="He docheshm",
        pronunciation="**h** in hat, or **e** in bed at the end of a word.",
    ),
    # Ye
    Letter(
        final="ـی",
        medial="ـیـ",
        initial="یـ",
        isolated="ی",
        name="Ye",
        pronunciation="Multiple: **i** kit, **e** in men,  /ye/ like **y** in yes.",
    ),
]

def letter_forms():
    print("Front,Back")
    for l in ALPHABET:
        print(f"\"What is the initial form of the Persian letter {l.name}?\",\"# {l.initial}\"")
        print(f"\"What is the medial form of the Persian letter {l.name}?\",\"# {l.medial}\"")
        print(f"\"What is the final form of the Persian letter {l.name}?\",\"# {l.final}\"")
        print(f"\"What is the isolated form of the Persian letter {l.name}?\",\"# {l.isolated}\"")

def identification():
    print("Front,Back")
    for l in ALPHABET:
        print(f"\"What is the name of this letter?\n# {l.initial}\",\"{l.name}\"")
        print(f"\"What is the name of this letter?\n# {l.medial}\",\"{l.name}\"")
        print(f"\"What is the name of this letter?\n# {l.final}\",\"{l.name}\"")
        print(f"\"What is the name of this letter?\n# {l.isolated}\",\"{l.name}\"")

def pronunciation():
    print("Front,Back")
    for l in ALPHABET:
        print(f"\"How is the letter {l.name} pronounced?\",\"{l.pronunciation}\"")

letter_forms()
identification()
pronunciation()
