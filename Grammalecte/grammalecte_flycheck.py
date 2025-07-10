# -*- coding: utf-8 -*-
import sys
from grammalecte import grammar_checker
import re


def mask_latex_commands(text):
    def replacer(match):
        s = match.group(0)
        # remplace chaque caractère par espace, sauf les retours
        # à la ligne conservés
        return "".join("\n" if c == "\n" else " " for c in s)

    # masque environnements \begin{…} … \end{…}
    text = re.sub(r"\\begin\{.*?\}.*?\\end\{.*?\}", replacer, text, flags=re.S)
    # masque commandes \command{…}
    text = re.sub(r"\\[a-zA-Z]+\{.*?\}", replacer, text)
    # masque commandes simples \command
    text = re.sub(r"\\[a-zA-Z]+", replacer, text)
    # masque maths inline $...$ ou $$...$$
    text = re.sub(r"(\${1,2})(.+?)\1", replacer, text, flags=re.S)
    # masque maths display \[ ... \]
    text = re.sub(r"\\\[(.*?)\\\]", replacer, text, flags=re.S)
    return text


IGNORED_PATTERNS = [
    "l’espace insécable",  # exemple
    "aucun message",  # exemple
    "Apostrophe typographique",
    "Espace·s surnuméraire·s à supprimer.",
    "Il manque un espace insécable.",
    "Pas d’espace avant un point.",
    "Pas d’espace avant une virgule.",
    "Pas d’espace avant ce signe.",
    "Pas d’espace après ce signe.",
    "Guillemets typographiques ouvrants.",
    "Guillemets typographiques fermants.",
]


def should_ignore(msg):
    return any(pat in msg for pat in IGNORED_PATTERNS)


def main():
    sys.stdin.reconfigure(encoding="utf-8")
    sys.stdout.reconfigure(encoding="utf-8")
    gc = grammar_checker.GrammarChecker("fr")

    text = sys.stdin.read()
    if not text.strip():
        print("<stdin>:1:1: error: Aucun texte fourni sur stdin.")
        return

    # # ignorer tout avant \begin{document}

    # idx = text.find(r"\begin{document}")
    # if idx >= 0:
    #     # Compte le nombre de lignes avant \begin{document}
    #     lines_before = text[:idx].count('\n') + 1
    #     # Masque les lignes avant \begin{document} par des espaces et retours à la ligne conservés
    #     masked_before = ''.join('\n' if c == '\n' else ' ' for c in text[:idx])
    #     # Compose nouveau texte avec la zone masquée + le reste
    #     masked_text = masked_before + text[idx:]
    # else:
    #     lines_before = 0
    #     masked_text = text

    text = mask_latex_commands(text)

    #    puis passer à grammalecte

    _, errors = gc.getParagraphWithErrors(text)
    lines = text.splitlines()

    for err in errors:
        start = err.get("nStart", 0)
        end = err.get("nEnd", start + 1)
        msg = err.get("sMessage")
        print(msg)
        if not msg:
            continue
        if should_ignore(msg):
            continue
        msg = msg.replace("\n", " ")

        # calcul ligne/colonne
        char_count = 0
        line_num = 1
        col_num = 1
        for line in lines:
            if char_count + len(line) + 1 > start:
                col_num = start - char_count + 1
                break
            char_count += len(line) + 1
            line_num += 1
        word = text[start:end]
        print(
            f"<stdin>:{line_num}:{col_num}-{col_num+(end-start)}: warning: {msg} [«{word}»]"
        )


if __name__ == "__main__":
    main()
